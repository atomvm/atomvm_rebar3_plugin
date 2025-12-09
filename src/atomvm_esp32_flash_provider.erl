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
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%
-module(atomvm_esp32_flash_provider).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-include_lib("kernel/include/file.hrl").

-define(PROVIDER, esp32_flash).
-define(DEPS, [packbeam]).
-define(OPTS, [
    {esptool, $e, "esptool", string, "Path to esptool.py"},
    {chip, $c, "chip", string, "ESP chip (default auto)"},
    {port, $p, "port", string, "Device port (default auto discovery)"},
    {baud, $b, "baud", integer, "Baud rate (default 115200)"},
    {offset, $o, "offset", string,
        "Offset (default read from device) *old behavior deprecated, use app_partition."
        " When given, verifies expected offset to actual"},
    {app_partition, $a, "app_partition", string, "Application partition name (default main.avm)"}
]).

-define(DEFAULT_OPTS, #{
    esptool => "esptool.py",
    chip => "auto",
    port => "auto",
    baud => "115200",
    offset => auto,
    app_partition => "main.avm"
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
        {example, "rebar3 atomvm esp32_flash"},
        % list of options understood by the plugin
        {opts, ?OPTS},
        {short_desc, "Flash an AtomVM packbeam to an ESP32 device"},
        {desc,
            "~n"
            "Use this plugin to flash an AtomVM packbeam file to an ESP32 device.~n"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    try
        Opts = get_opts(State),
        rebar_api:debug("Effective opts for ~p: ~p", [?PROVIDER, Opts]),
        ok = do_flash(
            rebar_state:project_apps(State),
            maps:get(esptool, Opts),
            maps:get(chip, Opts),
            maps:get(port, Opts),
            maps:get(baud, Opts),
            maybe_convert_string(maps:get(offset, Opts)),
            list_to_binary(maps:get(app_partition, Opts)),
            State
        ),
        {ok, State}
    catch
        C:rebar_abort:S ->
            rebar_api:error(
                "A fatal error occurred in the ~p task.",
                [?PROVIDER]
            ),
            rebar_api:debug("Class=~p, Error=~p~nSTACKTRACE:~n~p~n", [C, rebar_abort, S]),
            {error, rebar_abort};
        C:E:S ->
            rebar_api:error(
                "An unhandled error occurred in the ~p task.  Error=~p",
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
    rebar_api:debug("ParsedArgs = ~p", [ParsedArgs]),
    RebarOpts = atomvm_rebar3_plugin:get_atomvm_rebar_provider_config(State, ?PROVIDER),
    ParsedOpts = atomvm_rebar3_plugin:proplist_to_map(ParsedArgs),
    maps:merge(
        env_opts(),
        maps:merge(RebarOpts, ParsedOpts)
    ).

%% @private
env_opts() ->
    #{
        esptool => os:getenv(
            "ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_ESPTOOL",
            maps:get(esptool, ?DEFAULT_OPTS)
        ),
        chip => os:getenv(
            "ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_CHIP",
            maps:get(chip, ?DEFAULT_OPTS)
        ),
        port => os:getenv(
            "ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_PORT",
            maps:get(port, ?DEFAULT_OPTS)
        ),
        baud => maybe_convert_string(
            os:getenv(
                "ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_BAUD",
                maps:get(baud, ?DEFAULT_OPTS)
            )
        ),
        offset => maybe_convert_string(
            os:getenv(
                "ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_OFFSET",
                maps:get(offset, ?DEFAULT_OPTS)
            )
        ),
        app_partition => os:getenv(
            "ATOMVM_REBAR3_PLUGIN_ESP32_APP_PARTITION",
            maps:get(app_partition, ?DEFAULT_OPTS)
        )
    }.

%% @private
maybe_convert_string(S) when is_list(S) ->
    case lists:prefix("0x", S) of
        true ->
            list_to_integer(lists:subtract(S, "0x"), 16);
        false ->
            list_to_integer(S)
    end;
maybe_convert_string(I) ->
    I.

%% @private
do_flash(ProjectApps, EspTool, Chip, Port, Baud, Address, Partition, State) ->
    Offset =
        case Address of
            auto ->
                read_flash_offset(EspTool, Port, Partition, State);
            Val ->
                Offset0 = read_flash_offset(EspTool, Port, Partition, State),
                case Val =:= Offset0 of
                    true ->
                        Offset0;
                    false ->
                        rebar_api:abort(
                            "The configured offset 0x~.16B does not match the partition table on the device (0x~.16B).",
                            [Val, Offset0]
                        )
                end
        end,
    [ProjectAppAVM | _] = [get_avm_file(ProjectApp) || ProjectApp <- ProjectApps],
    Portparam =
        case Port of
            "auto" -> "";
            _ -> ["--port ", Port]
        end,
    Cmd = lists:join(" ", [
        EspTool,
        "--chip",
        Chip,
        Portparam,
        "--baud",
        integer_to_list(Baud),
        "--before",
        "default_reset",
        "--after",
        "hard_reset",
        "write_flash",
        "-u",
        "--flash_mode",
        "keep",
        "--flash_freq",
        "keep",
        "--flash_size",
        "detect",
        integer_to_list(Offset),
        ProjectAppAVM
    ]),
    AVMApp = filename:basename(ProjectAppAVM),
    rebar_api:info("Flashing ~s to device.", [AVMApp]),
    %% The following log output is parsed by the tests and should not be changed or removed.
    rebar_api:info("~s~n", [Cmd]),
    rebar_api:console("~s", [os:cmd(Cmd)]),
    ok.

%% @private
get_avm_file(App) ->
    OutDir = rebar_app_info:out_dir(App),
    Name = binary_to_list(rebar_app_info:name(App)),
    DirName = filename:dirname(OutDir),
    filename:join(DirName, Name ++ ".avm").

%% @private
read_flash_offset(Esptool, Port, PartName, State) ->
    TempFile = get_part_tempfile(State),
    rebar_api:info("Reading application partition offset from device...", []),
    try esp_part_dump:read_app_offset(Esptool, Port, PartName, TempFile) of
        Offset ->
            Offset
    catch
        _:invalid_partition_table:_ ->
            rebar_api:abort("Invalid partition data!", []);
        _:no_device:_ ->
            rebar_api:abort("No ESP32 device attached!", []);
        _:{partition_not_found, Partition}:_ ->
            rebar_api:error("The partition ~s was not fount on device partition table!", [Partition]),
            rebar_api:abort(
                "When using a custom partition table always specify the 'app_partition' NAME.", []
            );
        _:{invalid_subtype, Type}:_ ->
            rebar_api:abort("The partition ~s was found, but used invalid subtype 0x~s.", [
                PartName, Type
            ]);
        _:invalid_partition_data:_ ->
            rebar_api:abort("The partition ~s was found, but partition data is invalid.", [PartName]);
        _:Error:_ ->
            rebar_api:abort("Unexpected error reading partition table from device, ~p.", [Error])
    end.

%% @private
get_part_tempfile(State) ->
    OutDir = filename:absname(rebar_dir:base_dir(State)),
    TempFile = filename:absname_join(OutDir, "part.tmp"),
    case filelib:is_file(TempFile) of
        true ->
            rebar_api:debug("Removing possibly stale partition dump data ~s", [TempFile]),
            Cmd = lists:join(" ", ["rm", TempFile]),
            os:cmd(Cmd);
        false ->
            ok
    end,
    TempFile.
