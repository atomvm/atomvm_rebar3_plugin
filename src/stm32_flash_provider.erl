%%
%% Copyright (c) 2023 Uncle Grumpy <winford@object.stream>
%% All rights reserved.
%%
%% Based on esp32_flash_provider.erl
%% Copyright (c) dushin.net
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
-module(stm32_flash_provider).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-include_lib("kernel/include/file.hrl").

-define(PROVIDER, stm32_flash).
-define(DEPS, [packbeam]).
-define(OPTS, [
    {stflash, $s, "stflash", undefined, "Path to st-flash"},
    {offset, $o, "offset", undefined, "Offset (default 0x8080000)"}
]).

%%
%% provider implementation
%%
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        % The 'user friendly' name of the task
        {name, ?PROVIDER},
        % The module implementation of the task
        {module, ?MODULE},
        % The task can be run by the user, always true
        {bare, true},
        % The list of dependencies
        {deps, ?DEPS},
        % How to use the plugin
        {example, "rebar3 stm32_flash"},
        % list of options understood by the plugin
        {opts, ?OPTS},
        {short_desc, "A rebar plugin to flash packbeam to STM32 devices"},
        {desc, "A rebar plugin to flash packbeam to STM32 devices"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case parse_args(rebar_state:command_args(State)) of
        {ok, Opts} ->
            do_flash(
                rebar_state:project_apps(State),
                maps:get(
                    stflash,
                    Opts,
                    os:getenv(
                        "ATOMVM_REBAR3_PLUGIN_STM32_STFLASH",
                        "st-flash"
                    )
                ),
                maps:get(
                    offset, Opts, os:getenv("ATOMVM_REBAR3_PLUGIN_STM32_FLASH_OFFSET", "0x8080000")
                )
            ),
            {ok, State};
        {error, Reason} ->
            io:format("~p~n", [Reason]),
            {error, Reason}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%
%% internal functions
%%

%% @private
parse_args(Args) ->
    parse_args(Args, #{external_avms => [], prune => false, force => false}).

%% @private
parse_args([], Accum) ->
    {ok, Accum};
parse_args(["-s", StFlash | Rest], Accum) ->
    parse_args(Rest, Accum#{stflash => StFlash});
parse_args(["--stflash", StFlash | Rest], Accum) ->
    parse_args(Rest, Accum#{stflash => StFlash});
parse_args(["-o", Offset | Rest], Accum) ->
    parse_args(Rest, Accum#{offset => Offset});
parse_args(["--offset", Offset | Rest], Accum) ->
    parse_args(Rest, Accum#{offset => Offset});
parse_args([_ | Rest], Accum) ->
    parse_args(Rest, Accum).

%% @private
do_flash(ProjectApps, StFlash, Offset) ->
    [ProjectAppAVM | _] = [get_avm_file(ProjectApp) || ProjectApp <- ProjectApps],
    Cmd = lists:join(" ", [
        StFlash,
        "--reset",
        "write",
        ProjectAppAVM,
        Offset
    ]),
    try
        rebar_api:info("~s~n", [Cmd]),
        io:format("~s", [os:cmd(Cmd)])
    catch
        _:Error ->
            rebar_api:error("Error executing ~p.  Error: ~p", [Cmd, Error])
    end.

%% @private
get_avm_file(App) ->
    OutDir = rebar_app_info:out_dir(App),
    Name = binary_to_list(rebar_app_info:name(App)),
    DirName = filename:dirname(OutDir),
    filename:join(DirName, Name ++ ".avm").
