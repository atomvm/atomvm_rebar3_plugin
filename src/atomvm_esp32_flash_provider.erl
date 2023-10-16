%%
%% Copyright (c) dushin.net
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
-module(atomvm_esp32_flash_provider).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-include_lib("kernel/include/file.hrl").

-define(PROVIDER, esp32_flash).
-define(DEPS, [packbeam]).
-define(OPTS, [
    {esptool, $e, "esptool", string, "Path to esptool.py"},
    {chip, $c, "chip", string, "ESP chip (default auto)"},
    {port, $p, "port", string, "Device port (default /dev/ttyUSB0)"},
    {baud, $b, "baud", integer, "Baud rate (default 115200)"},
    {offset, $o, "offset", string, "Offset (default 0x210000)"}
]).

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
        {example, "rebar3 atomvm esp32_flash"},
        % list of options understood by the plugin
        {opts, ?OPTS},
        {short_desc, "A rebar plugin to flash packbeam to ESP32 devices"},
        {desc, "A rebar plugin to flash packbeam to ESP32 devices"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    try
        Opts = get_opts(rebar_state:command_parsed_args(State)),
        ok = do_flash(
            rebar_state:project_apps(State),
            maps:get(
                esptool,
                Opts,
                os:getenv(
                    "ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_ESPTOOL",
                    "esptool.py"
                )
            ),
            maps:get(
                chip, Opts,
                os:getenv("ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_CHIP", "auto")
            ),
            maps:get(
                port, Opts,
                os:getenv("ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_PORT", "/dev/ttyUSB0")
            ),
            maps:get(
                baud, Opts,
                list_to_integer(os:getenv("ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_BAUD", "115200"))
            ),
            maps:get(
                offset, Opts,
                os:getenv("ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_OFFSET", "0x210000")
            )
        ),
        {ok, State}
    catch
        _:E ->
            rebar_api:error("~p~n", [E]),
            {error, E}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%
%% internal functions
%%

%% @private
get_opts({ParsedArgs, _}) ->
    atomvm_rebar3_plugin:proplist_to_map(ParsedArgs).

%% @private
do_flash(ProjectApps, EspTool, Chip, Port, Baud, Offset) ->
    [ProjectAppAVM | _] = [get_avm_file(ProjectApp) || ProjectApp <- ProjectApps],
    Cmd = lists:join(" ", [
        EspTool,
        "--chip", Chip,
        "--port", Port,
        "--baud", integer_to_list(Baud),
        "--before", "default_reset",
        "--after", "hard_reset",
        "write_flash",
        "-u",
        "--flash_mode", "keep",
        "--flash_freq", "keep",
        "--flash_size", "detect",
        Offset,
        ProjectAppAVM
    ]),
    rebar_api:info("~s~n", [Cmd]),
    rebar_api:console("~s", [os:cmd(Cmd)]),
    ok.

%% @private
get_avm_file(App) ->
    OutDir = rebar_app_info:out_dir(App),
    Name = binary_to_list(rebar_app_info:name(App)),
    DirName = filename:dirname(OutDir),
    filename:join(DirName, Name ++ ".avm").
