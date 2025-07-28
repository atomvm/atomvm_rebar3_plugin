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
    esptool => default_esptool(),
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

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, term()}.
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
            maps:get(offset, Opts),
            maps:get(app_partition, Opts)
        ),
        {ok, State}
    catch
        throw:rebar_abort:S ->
            rebar_api:error(
                "A fatal error occurred in the ~p task.",
                [?PROVIDER]
            ),
            rebar_api:debug("Class=~p, Error=~p~nSTACKTRACE:~n~p~n", [throw, rebar_abort, S]),
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
default_esptool() ->
    case os:find_executable("esptool.py") of
        false ->
            case os:find_executable("esptool") of
                false ->
                    %% let external_command produce {enoent, ...}
                    "esptool.py";
                Path ->
                    Path
            end;
        Path ->
            Path
    end.

%% @private
get_opts(State) ->
    {ParsedArgs, _} = rebar_state:command_parsed_args(State),
    rebar_api:debug("ParsedArgs = ~w", [ParsedArgs]),
    RebarOpts = atomvm_rebar3_plugin:get_atomvm_rebar_provider_config(State, ?PROVIDER),
    ParsedOpts = atomvm_rebar3_plugin:proplist_to_map(ParsedArgs),
    Merged = maps:merge(maps:merge(env_opts(), RebarOpts), ParsedOpts),
    Merged#{
        baud := maybe_integer_from_string(maps:get(baud, Merged)),
        offset := maybe_integer_from_string(maps:get(offset, Merged))
    }.

%% @private
env_opts() ->
    Defaults = ?DEFAULT_OPTS,
    #{
        esptool => os:getenv(
            "ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_ESPTOOL",
            maps:get(esptool, Defaults)
        ),
        chip => os:getenv(
            "ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_CHIP",
            maps:get(chip, Defaults)
        ),
        port => os:getenv(
            "ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_PORT",
            maps:get(port, Defaults)
        ),
        baud => maybe_integer_from_string(
            os:getenv(
                "ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_BAUD",
                maps:get(baud, Defaults)
            )
        ),
        offset => maybe_integer_from_string(
            os:getenv(
                "ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_OFFSET",
                maps:get(offset, Defaults)
            )
        ),
        app_partition => os:getenv(
            "ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_APP_PARTITION",
            maps:get(app_partition, Defaults)
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
do_flash(ProjectApps, EspTool, Chip, Port, Baud, Offset, Partition) ->
    TempDir = atomvm_rebar3_plugin:create_temp_dir("atomvm_esp32_flash.XXXXXX"),
    TempFile = filename:join(TempDir, "partitions.bin"),
    EspToolLog = filename:join(TempDir, "esptool.log"),

    %% Classify the run before unwinding: the retained artifacts depend on how
    %% the run ended, and decode_abort_reason/1 throws rather than returns.
    Result =
        try
            flash(
                ProjectApps, EspTool, Chip, Port, Baud, Offset, Partition, TempFile, EspToolLog
            ),
            success
        catch
            error:Reason ->
                {error, Reason};
            C:E:S ->
                {error, {C, E, S}}
        end,

    cleanup_artifacts(TempDir, TempFile, EspToolLog, Result),

    case Result of
        success ->
            rebar_api:info("Success!", []),
            ok;
        {error, Error} ->
            decode_abort_reason(Error)
    end.

%% @private
flash(ProjectApps, EspTool, Chip, Port, Baud, Offset, Partition, TempFile, EspToolLog) ->
    Address =
        case Offset of
            auto ->
                read_flash_offset(EspTool, Chip, Port, Baud, Partition, TempFile, EspToolLog);
            Val ->
                case
                    read_flash_offset(
                        EspTool, Chip, Port, Baud, Partition, TempFile, EspToolLog
                    )
                of
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

    ok = atomvm_rebar3_plugin:external_command(EspTool, Args, EspToolLog),
    ok.

%% Artifact retention policy.
%%
%% A successful run leaves nothing behind.
%%
%% When the flash tool itself fails, the tool log is the only useful artifact, so
%% it is always retained and the (possibly truncated) partition dump is deleted.
%%
%% When a dump was taken but could not be used, the dump is the evidence needed to
%% explain the failure, so it is retained along with the tool log.
%%
%% Any other failure is unexpected: both artifacts are retained, and the caller is
%% told where they are.
%%
%% The test suite deliberately provokes the `anticipated' causes above (a missing
%% partition, a bad subtype, a non-data partition, a tool that exits non-zero or
%% hangs) and asserts on the resulting message, so those runs retain nothing.  A
%% corrupt or unreadable dump is NOT anticipated, so it still retains artifacts
%% during tests, exactly as it does in production.
%% @private
cleanup_artifacts(TempDir, TempFile, EspToolLog, Result) ->
    {Cause, Retain} = artifact_policy(Result),
    Retain1 =
        case test_mode() andalso anticipated(Cause) of
            true -> none;
            false -> Retain
        end,

    KeepLog = lists:member(Retain1, [log, log_dump]),
    KeepDump = lists:member(Retain1, [log_dump]),

    delete_unless(KeepDump, TempFile),
    delete_unless(KeepLog, EspToolLog),

    case {KeepLog, KeepDump} of
        {false, false} ->
            %% Temp dir holds only files we just removed; del_dir_r also cleans up
            %% if a delete above failed, so success never leaves a dir behind.
            file:del_dir_r(TempDir);
        _ ->
            report_retained(EspToolLog, TempFile, KeepLog, KeepDump)
    end,
    ok.

%% @private
%% Returns {Cause, Retain}, where Retain is `none', `log' or `log_dump'.
artifact_policy(success) ->
    {success, none};
artifact_policy({error, {_, {exit_status, _}, _}}) ->
    %% esptool ran and reported a failure
    {tool_failure, log};
artifact_policy({error, {timeout, _, over_2_minutes}}) ->
    {tool_failure, log};
artifact_policy({error, {enoent, _}}) ->
    {tool_failure, log};
artifact_policy({error, {eacces, _}}) ->
    {tool_failure, log};
artifact_policy({error, no_device_dump}) ->
    {dump_failure, log_dump};
artifact_policy({error, no_dump_file}) ->
    {dump_failure, log_dump};
artifact_policy({error, {partition_not_found, _}}) ->
    {dump_lookup, log_dump};
artifact_policy({error, {invalid_subtype, _}}) ->
    {dump_lookup, log_dump};
artifact_policy({error, {invalid_partition_type, _}}) ->
    {dump_lookup, log_dump};
artifact_policy({error, {invalid_partition, _}}) ->
    {dump_lookup, log_dump};
artifact_policy({error, corrupt_partition_table}) ->
    %% a corrupt dump is never an expected test outcome; keep the evidence
    {dump_corrupt, log_dump};
artifact_policy({error, {file_read, _}}) ->
    {dump_corrupt, log_dump};
artifact_policy({error, {_, _, _, _, _}}) ->
    %% the tool could not be spawned at all
    {spawn_failure, log_dump};
artifact_policy({error, _}) ->
    {unexpected, log_dump}.

%% Causes the test suite provokes on purpose and asserts on; nothing to debug.
%% @private
anticipated(tool_failure) -> true;
anticipated(dump_lookup) -> true;
anticipated(_) -> false.

%% @private
report_retained(EspToolLog, TempFile, KeepLog, KeepDump) ->
    Candidates = [Path || {true, Path} <- [{KeepLog, EspToolLog}, {KeepDump, TempFile}]],
    Retained = [Path || Path <- Candidates, filelib:is_regular(Path)],
    case Retained of
        [] ->
            ok;
        _ ->
            rebar_api:warn(
                "Temporary artifacts retained: ~s",
                [lists:flatten(lists:join(", ", Retained))]
            )
    end.

%% @private
test_mode() ->
    os:getenv("TEST") =:= "1".

%% @private
delete_unless(false, Path) ->
    delete_quietly(Path);
delete_unless(true, _Path) ->
    ok.

%% @private
delete_quietly(Path) ->
    _ = file:delete(Path),
    ok.

%% @private
get_avm_file(App) ->
    OutDir = rebar_app_info:out_dir(App),
    Name = binary_to_list(rebar_app_info:name(App)),
    DirName = filename:dirname(OutDir),
    filename:join(DirName, Name ++ ".avm").

%% The caller (do_flash/7) owns error reporting, so these helpers let the raw
%% failure reason propagate instead of aborting with a decoded message.  Decoding
%% here would both hide the reason from the artifact retention policy and
%% re-wrap the abort into a misleading "unexpected error" message.
%% @private
read_flash_offset(EspTool, Chip, Port, Baud, PartName, TempFile, EspToolLog) ->
    rebar_api:info("Reading application partition offset from device...", []),
    esp_part_dump:get_app_offset(EspTool, Chip, Port, Baud, PartName, TempFile, EspToolLog).

%% @private
validate_offset(Offset, Partition, TempFile) ->
    rebar_api:debug("Checking that offset aligns with partition from table on device...", []),
    case esp_part_dump:partition_at_offset(Offset, TempFile) of
        {Found, _Type} ->
            rebar_api:warn(
                "The discovered partition ~s at offset 0x~.16B will be used, not the expected partition named ~s.",
                [Found, Offset, Partition]
            ),
            {ok, Offset}
    end.

%% @private
decode_abort_reason(Reason) ->
    case Reason of
        no_device_dump ->
            rebar_api:abort("No partition table retrieved from device.", []);
        no_dump_file ->
            rebar_api:abort("No partition dump file available.", []);
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
        corrupt_partition_table ->
            rebar_api:abort("Fatal error! Corrupt partition table data!", []);
        {file_read, Error} ->
            rebar_api:abort("Unable to read partition table data: ~p", [Error]);
        {"esptool failure", Status} ->
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
        {timeout, Name, over_2_minutes} ->
            rebar_api:abort("External command ~s failed (2 minute timeout exceeded)", [Name]);
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
