%%
%% Copyright (c) 2020-2023 Fred Dushin <fred@dushin.net>
%% All rights reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%%
-module(atomvm_rebar3_plugin).

-export([init/1]).

%% internal API
-export([proplist_to_map/1, get_atomvm_rebar_provider_config/2, external_command/3]).

-define(PROVIDERS, [
    atomvm_bootstrap_provider,
    atomvm_packbeam_provider,
    atomvm_dialyzer_provider,
    atomvm_escriptize_provider,
    atomvm_esp32_flash_provider,
    atomvm_pico_flash_provider,
    atomvm_stm32_flash_provider,
    atomvm_uf2create_provider,
    atomvm_version_provider,
    legacy_packbeam_provider,
    legacy_esp32_flash_provider,
    legacy_stm32_flash_provider
]).

-type proplist() :: [{term(), term()} | term()].

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    lists:foldl(
        fun(Cmd, {ok, StateAcc}) ->
            apply(Cmd, init, [StateAcc])
        end,
        {ok, State},
        ?PROVIDERS
    ).

-spec proplist_to_map(Proplist :: proplist()) -> map().
proplist_to_map(Proplist) ->
    proplist_to_map(Proplist, #{}).

%% @private
proplist_to_map([], Accum) ->
    Accum;
proplist_to_map([{K, V} | T], Accum) ->
    proplist_to_map(T, Accum#{K => V});
proplist_to_map([K | T], Accum) ->
    proplist_to_map(T, Accum#{K => true}).

-spec get_atomvm_rebar_provider_config(State :: term(), Provider :: atom()) -> map().
get_atomvm_rebar_provider_config(State, Provider) ->
    case rebar_state:get(State, ?MODULE, undefined) of
        undefined ->
            #{};
        AtomVM ->
            proplist_to_map(proplists:get_value(Provider, AtomVM, []))
    end.

-spec external_command(
    Cmd :: file:name_all(), Args :: [string()], LogFile :: file:name_all() | none
) -> Result :: ok | {ok, iodata()}.
external_command(Cmd, Args, LogFile) ->
    CmdName = filename:basename(Cmd),
    case LogFile of
        none ->
            ok;
        _ ->
            {{Y, M, D}, {H, I, S}} = calendar:local_time(),
            LogBegin = io_lib:format("~s executed at ~p/~p/~p ~p:~p:~p~n", [
                CmdName, Y, M, D, H, I, S
            ]),
            file:write_file(LogFile, LogBegin, [append, {encoding, utf8}])
    end,
    CmdPort =
        try
            open_port({spawn_executable, Cmd}, [
                {args, Args}, stderr_to_stdout, use_stdio, exit_status
            ])
        catch
            error:enoent ->
                error({enoent, CmdName});
            error:eacces ->
                error({eacces, Cmd});
            C:E:Trace ->
                error({Cmd, Args, C, E, Trace})
        end,
    case LogFile of
        none ->
            wait_for_exit(CmdPort, CmdName, []);
        _ ->
            log_command_output(CmdPort, CmdName, LogFile)
    end.

-spec wait_for_exit(CmdPort :: port(), CmdName :: string(), Output :: iodata()) -> iodata().
wait_for_exit(CmdPort, CmdName, Output) ->
    receive
        {CmdPort, {data, Data}} ->
            wait_for_exit(CmdPort, CmdName, [Data | Output]);
        {CmdPort, {exit_status, 0}} ->
            {ok, string:trim(lists:flatten(lists:reverse(Output)))};
        {CmdPort, {exit_status, Status}} ->
            error({CmdName, {exit_status, Status}})
    after 300000 ->
        port_close(CmdPort),
        error({timeout, CmdName, min_5})
    end.

-spec log_command_output(CmdPort :: port(), CmdName :: string(), LogFile :: file:name_all()) -> ok.
log_command_output(CmdPort, CmdName, LogFile) ->
    receive
        {CmdPort, {data, Data}} ->
            file:write_file(LogFile, Data, [append, {encoding, utf8}]),
            log_command_output(CmdPort, CmdName, LogFile);
        {CmdPort, {exit_status, 0}} ->
            file:delete(LogFile),
            ok;
        {CmdPort, {exit_status, Status}} ->
            Msg = io_lib:format("Command ~s failed! Exit status ~p.~n", [CmdName, Status]),
            file:write_file(LogFile, Msg, [append, {encoding, utf8}]),
            error({CmdName, {exit_status, Status}, LogFile})
    after 300000 ->
        port_close(CmdPort),
        error({timeout, CmdName, min_5})
    end.
