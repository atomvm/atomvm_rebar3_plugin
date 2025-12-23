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
        "Offset (default read from device) *deprecated, use app_partition. "
        "When given, a warning will be issued if the partition name does not match the "
        "expected name, as long as the offset aligns with a valid application partition."},
    {app_partition, $a, "app_partition", string, "Application partition name (default main.avm)"}
]).

-define(DEFAULT_OPTS, #{
    esptool => os:find_executable("esptool.py"),
    chip => "auto",
    port => "auto",
    baud => "115200",
    offset => "auto",
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
            maybe_integer_from_string(maps:get(offset, Opts)),
            maps:get(app_partition, Opts),
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
            rebar_api:debug("Class=~p, Error=~p~nSTACKTRACE:~n~p~n", [C, E, S]),
            rebar_api:abort(
                "An unhandled error occurred in the ~p task.  Error=~p",
                [?PROVIDER, E]
            ),
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
    rebar_api:debug("ParsedArgs = ~w", [ParsedArgs]),
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
        baud => maybe_integer_from_string(
            os:getenv(
                "ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_BAUD",
                maps:get(baud, ?DEFAULT_OPTS)
            )
        ),
        offset => maybe_integer_from_string(
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
-spec maybe_integer_from_string(S :: list() | integer() | auto) -> Value :: integer() | auto.
maybe_integer_from_string(auto) ->
    auto;
maybe_integer_from_string("auto") ->
    auto;
maybe_integer_from_string(I) when is_integer(I) ->
    I;
maybe_integer_from_string(S) when is_list(S) ->
    S0 = string:lowercase(string:trim(S)),
    case lists:prefix("0x", S0) of
        true ->
            IntStr = lists:sublist(S0, 3, length(S0)),
            list_to_integer(IntStr, 16);
        false ->
            list_to_integer(S0)
    end.

%% @private
do_flash(ProjectApps, EspTool, Chip, Port, Baud, Offset, Partition, State) ->
    {TempFile, EspToolLog} = get_part_tempfiles(),
    Address =
        case Offset of
            auto ->
                read_flash_offset(EspTool, Port, Partition, TempFile, EspToolLog);
            Val ->
                case read_flash_offset(EspTool, Port, Partition, TempFile, EspToolLog) of
                    Val ->
                        Val;
                    _Other ->
                        {ok, Offset1} = validate_offset(Val, Partition, TempFile),
                        Offset1
                end
        end,
    ProjectAppAVM = get_avm_file(hd(ProjectApps)),

    StdArgs = [
        "--chip",
        Chip,
        "--baud",
        integer_to_list(Baud),
        "--before",
        "default_reset",
        "--after",
        "hard_reset",
        "write_flash",
        "--flash_mode",
        "keep",
        "--flash_freq",
        "keep",
        "--flash_size",
        "detect",
        "0x" ++ integer_to_list(Address, 16),
        ProjectAppAVM
    ],
    Args =
        case Port of
            "auto" ->
                StdArgs;
            _ ->
                ["--port", Port | StdArgs]
        end,

    AVMApp = filename:basename(ProjectAppAVM),
    rebar_api:info("Flashing ~s to device.", [AVMApp]),

    %% The following log output is parsed by the tests and should not be changed or removed.
    case os:getenv("TEST") of
        false ->
            ok;
        _ ->
            rebar_api:info("~s ~s", [EspTool, lists:flatten(lists:join(" ", Args))])
    end,

    try atomvm_rebar3_plugin:external_command(EspTool, Args, EspToolLog) of
        ok -> ok
    catch
        error:Reason ->
            decode_abort_reason(Reason);
        C:E:S ->
            decode_abort_reason({C, E, S})
    end,
    rebar_api:info("Success!", []),

    file:delete(TempFile),
    ok.

%% @private
get_avm_file(App) ->
    OutDir = rebar_app_info:out_dir(App),
    Name = binary_to_list(rebar_app_info:name(App)),
    DirName = filename:dirname(OutDir),
    filename:join(DirName, Name ++ ".avm").

%% @private
read_flash_offset(EspTool, Port, PartName, TempFile, EspToolLog) ->
    rebar_api:info("Reading application partition offset from device...", []),
    try esp_part_dump:read_app_offset(EspTool, Port, PartName, TempFile, EspToolLog) of
        Offset ->
            Offset
    catch
        error:Reason ->
            decode_abort_reason(Reason);
        C:E:S ->
            decode_abort_reason({C, E, S})
    end.

validate_offset(Offset, Partition, TempFile) ->
    rebar_api:debug("Checking that offset aligns with partition from table on device...", []),
    try esp_part_dump:partition_at_offset(Offset, TempFile) of
        {Found, _Type} ->
            rebar_api:warn(
                "The discovered partition ~s at offset 0x~.16B will be used, not the expected partition named ~s.",
                [Found, Offset, Partition]
            ),
            {ok, Offset}
    catch
        error:Reason ->
            decode_abort_reason(Reason);
        C:E:S ->
            decode_abort_reason({C, E, S})
    end.

%% @private
get_part_tempfiles() ->
    TempFile = create_temp_file("partitions.bin.XXXXX"),
    %% we are generating the file name as a side-effect, the partition table dump file should not actually exist yet.
    file:delete(TempFile),

    rebar_api:debug("Using partition dump file ~s.", [TempFile]),

    Logfile = create_temp_file("esptool.log.XXXXX"),
    rebar_api:debug("Using esptool.py logfile ~s.", [Logfile]),

    {TempFile, Logfile}.

create_temp_file(Format) ->
    Cmd = os:find_executable("mktemp"),
    Args = ["-q", Format],
    {ok, TempFile} = atomvm_rebar3_plugin:external_command(Cmd, Args, none),
    TempFile.

decode_abort_reason(Reason) ->
    case Reason of
        invalid_partition_table ->
            rebar_api:abort("Invalid partition data!", []);
        no_device_dump ->
            rebar_api:abort("No partition table retrieved from device.", []);
        {partition_not_found, Partition} ->
            rebar_api:error("The partition ~s was not found in device partition table!", [Partition]),
            rebar_api:abort(
                "When using a custom partition table always specify the 'app_partition' NAME.", []
            );
        {invalid_subtype, {PartName, Type}} ->
            rebar_api:abort("The partition ~s was found, but used invalid subtype ~w.", [
                PartName, Type
            ]);
        {invalid_partition_type, PartName} ->
            rebar_api:abort(
                "The partition ~s is not a data partition!~nOnly data partitions may be used for AtomVM applications",
                [
                    PartName
                ]
            );
        corrupt_partition_data ->
            rebar_api:abort("Fatal error! Corrupt partition table data!", []);
        no_data_partitions ->
            rebar_api:abort("The partition table on the device contains no data partitions");
        {"esptool.py failure", Status} ->
            rebar_api:abort(
                "An error was encountered connecting to device with esptool.py (error ~p)\n"
                "Is minicom or serial monitor attached?",
                [Status]
            );
        {invalid_partition, {Offset, Name, Reason}} ->
            rebar_api:abort(
                "The configured offset 0x~.16B contains partition ~s, which cannot be used for reason: ~p.~n",
                [Offset, Name, Reason]
            );
        %% we also want to match on esptool without .py extension for some distributions of the tool, and for .sh
        %% extension for the test suite.
        {[$e, $s, $p, $t, $o, $o, $l | _], {exit_status, 2}, LogFile} ->
            rebar_api:abort(
                "Could not establish communication with ESP32 device! Is serial monitor attached?~nLogfile: ~s",
                [LogFile]
            );
        {Name, {exit_status, Status}, LogFile} ->
            rebar_api:abort("External command ~s exited with error ~w~nLogfile saved: ~s", [
                Name, Status, LogFile
            ]);
        {timeout, Name, min_5} ->
            rebar_api:abort("External command ~s failed (5 minute timeout exceeded)", [Name]);
        {enoent, CmdName} ->
            rebar_api:abort("external command ~s not found", [CmdName]);
        {eacces, Cmd} ->
            rebar_api:abort("The file at path ~s is not executable, or user lacks permission", [Cmd]);
        {Cmd, Args, C, E, Trace} ->
            rebar_api:abort(
                "Unexpected error using external command ~s (Args = ~s)~nClass = ~p, Error = ~p~nSTACKTRACE:~n~p~n",
                [Cmd, Args, C, E, Trace]
            );
        {C, E, S} ->
            rebar_api:debug("Class=~p, Error=~p~nSTACKTRACE:~n~p~n", [C, E, S]),
            rebar_api:abort(
                "Unexpected error reading partition table from device. Error = ~p.",
                [E]
            );
        Reason ->
            rebar_api:abort("Unexpected abort! Reason ~w", [Reason])
    end.
