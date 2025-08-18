%
% This file is part of AtomVM.
%
% Copyright 2023-24 Winford (UncleGrumpy) <winford@object.stream>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(atomvm_uf2create_provider).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-include_lib("kernel/include/file.hrl").

-define(PROVIDER, uf2create).
-define(DEPS, [packbeam]).
-define(OPTS, [
    {family_id, $f, "family_id", string,
        "Device family or flavor of uf2 file to create (default universal)"},
    {output, $o, "output", string, "Output path/name"},
    {start, $s, "start", string, "Start address for the uf2 binary (default 0x10180000)"},
    {input, $i, "input", string, "Input avm file to convert to uf2"}
]).

-define(DEFAULT_OPTS, #{
    start => os:getenv("ATOMVM_PICO_APP_START", "0x10180000"),
    family_id => os:getenv("ATOMVM_PICO_UF2_FAMILY", "universal")
}).

%%
%% provider implementation
%%
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        % The atomvm namespace
        {namespace, atomvm},
        % The 'user friendly' name of the task
        {name, ?PROVIDER},
        % The module implementation of the task
        {module, ?MODULE},
        % The task can be run by the user, always true
        {bare, true},
        % The list of dependencies
        {deps, ?DEPS},
        % How to use the plugin
        {example, "rebar3 atomvm uf2create"},
        % list of options understood by the plugin
        {opts, ?OPTS},
        {short_desc, "Create a Raspberry Pico uf2 file from an AtomVM packbeam file"},
        {desc,
            "~n"
            "Use this plugin to create Raspberry Pico uf2 files from an AtomVM packbeam file.~n"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    try
        Opts = get_opts(State),
        rebar_api:debug("Effective opts for ~p: ~p", [?PROVIDER, Opts]),
        OutFile = get_out_file(State),
        TargetAVM = get_avm_file(State),
        Output = maps:get(output, Opts, OutFile),
        StartAddrStr = parse_addr(maps:get(start, Opts)),
        Image = maps:get(input, Opts, TargetAVM),
        Uf2Flavor = validate_flavor(maps:get(family_id, Opts)),
        ok = uf2tool:uf2create(Output, Uf2Flavor, StartAddrStr, Image),
        {ok, State}
    catch
        C:E:S ->
            rebar_api:error(
                "An error occurred in the ~p task.  Error=~p",
                [?PROVIDER, E]
            ),
            rebar_api:debug("Class=~p, Error=~p~nSTACKTRACE:~n~p~n", [C, E, S]),
            {error, E}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%
%% internal functions
%%

%% @private
get_opts(State) ->
    {ParsedArgs, _} = rebar_state:command_parsed_args(State),
    RebarOpts = atomvm_rebar3_plugin:get_atomvm_rebar_provider_config(State, ?PROVIDER),
    ParsedOpts = atomvm_rebar3_plugin:proplist_to_map(ParsedArgs),
    maps:merge(
        env_opts(),
        maps:merge(RebarOpts, ParsedOpts)
    ).

%% @private
env_opts() ->
    #{
        start => os:getenv(
            "ATOMVM_REBAR3_PLUGIN_UF2CREATE_START",
            maps:get(start, ?DEFAULT_OPTS)
        ),
        family_id => os:getenv(
            "ATOMVM_REBAR3_PLUGIN_UF2_FAMILY",
            maps:get(family_id, ?DEFAULT_OPTS)
        )
    }.

%% @private
get_avm_file(State) ->
    [App] = [ProjectApp || ProjectApp <- rebar_state:project_apps(State)],
    OutDir = rebar_app_info:out_dir(App),
    Name = binary_to_list(rebar_app_info:name(App)),
    DirName = filename:dirname(OutDir),
    filename:join(DirName, Name ++ ".avm").

%% @private
get_out_file(State) ->
    [App] = [ProjectApp || ProjectApp <- rebar_state:project_apps(State)],
    OutDir = rebar_app_info:out_dir(App),
    Name = binary_to_list(rebar_app_info:name(App)),
    DirName = filename:dirname(OutDir),
    filename:join(DirName, Name ++ ".uf2").

%% @private
parse_addr("0x" ++ AddrHex) ->
    list_to_integer(AddrHex, 16);
parse_addr("16#" ++ AddrHex) ->
    list_to_integer(AddrHex, 16);
parse_addr(AddrDec) ->
    list_to_integer(AddrDec).

%% @private
validate_flavor(Flavor) ->
    case Flavor of
        rp2040 ->
            rp2040;
        "rp2040" ->
            rp2040;
        rp2350 ->
            data;
        "rp2350" ->
            data;
        data ->
            data;
        "data" ->
            data;
        universal ->
            universal;
        "universal" ->
            universal;
        Family ->
            rebar_api:error("An error occurred in the ~p task. Invalid family_id ~p~n", [
                ?PROVIDER, Family
            ])
    end.
