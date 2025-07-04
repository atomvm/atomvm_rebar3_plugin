%%
%% Copyright (c) 2023-2024 Uncle Grumpy <winford@object.stream>
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
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%%
-module(atomvm_pico_flash_provider).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-include_lib("kernel/include/file.hrl").

-define(PROVIDER, pico_flash).
-define(DEPS, [uf2create]).
-define(OPTS, [
    {path, $p, "path", string,
        "Path to pico device (Defaults Linux: /run/media/${USER}/RPI-RP2, MacOS: /Volumes/RPI-RP2)"},
    {reset, $r, "reset", string,
        "Path to serial device to reset before flashing (Defaults Linux: /dev/ttyACM0, MacOS: /dev/cu.usbmodem14*)"}
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
        {example, "rebar3 atomvm pico_flash"},
        % list of options understood by the plugin
        {opts, ?OPTS},
        {short_desc, "Convert an AtomVM packbeam file to uf2 and copy to an rp2040 device"},
        {desc,
            "~n"
            "Use this plugin to convert an AtomVM packbeam file to a uf2 file and copy to an rp2040 device.~n"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    try
        Opts = get_opts(State),
        rebar_api:debug("Effective opts for ~p: ~p", [?PROVIDER, Opts]),
        ok = do_flash(
            rebar_state:project_apps(State),
            maps:get(path, Opts),
            maps:get(reset, Opts)
        ),
        {ok, State}
    catch
        _C:E:_S ->
            rebar_api:error("An error occurred in the ~p task.  Error=~p~n", [?PROVIDER, E]),
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
        path => os:getenv(
            "ATOMVM_REBAR3_PLUGIN_PICO_MOUNT_PATH",
            os:getenv("ATOMVM_PICO_MOUNT_PATH", "")
        ),
        reset => os:getenv(
            "ATOMVM_REBAR3_PLUGIN_PICO_RESET_DEV",
            get_reset_dev()
        )
    }.

%% @private
get_stty_file_flag() ->
    {_Fam, System} = os:type(),
    case System of
        linux ->
            "-F";
        _Other ->
            "-f"
    end.

%% @private
get_reset_dev() ->
    {_Fam, System} = os:type(),
    Base =
        case System of
            linux ->
                filelib:wildcard("/dev/serial/by-id/usb-Raspberry_Pi_Pico_????????????????-if00");
            darwin ->
                Bin = darwin_ioreg:pico_callout_devices(),
                lists:foldl(fun(Path, Acc) -> [binary_to_list(Path) | Acc] end, [], Bin);
            _Other ->
                []
        end,
    os:getenv("ATOMVM_PICO_RESET_DEV", Base).

%%% @private
find_mounted_pico() ->
    {_Fam, System} = os:type(),
    Wildcard =
        case System of
            linux ->
                "/run/media/" ++ os:getenv("USER") ++ "/{RPI-RP2,RP2350}";
            darwin ->
                "/Volumes/{RPI-RP2,RP2350}";
            _Other ->
                ""
        end,
    case filelib:wildcard(Wildcard) of
        [Pico | _] = Path ->
            rebar_api:debug("Found pico device, using ~p from devices list: ~p", [Pico, Path]),
            {ok, Pico};
        _ ->
            not_found
    end.

%% @private
wait_for_mount(Mount, Count) when Count < 30 ->
    case Mount of
        "" ->
            case find_mounted_pico() of
                not_found ->
                    timer:sleep(1000),
                    wait_for_mount(Mount, Count + 1);
                {ok, Pico} ->
                    case file:read_link_info(Pico) of
                        {ok, #file_info{type = directory} = _Info} ->
                            {ok, Pico};
                        Err ->
                            rebar_api:abort("Pico mount point is not a directory (~p)", [Err])
                    end
            end;
        Path ->
            case file:read_link_info(Path) of
                {ok, #file_info{type = directory} = _Info} ->
                    {ok, Path};
                _ ->
                    timer:sleep(1000),
                    wait_for_mount(Mount, Count + 1)
            end
    end;
wait_for_mount(_Mount, 30) ->
    rebar_api:abort("Pico not mounted after 30 seconds. giving up...", []).

%% @private
get_pico_mount(Mount) ->
    case Mount of
        "" ->
            case find_mounted_pico() of
                not_found ->
                    rebar_api:info("Waiting for an RP2 device to mount...", []),
                    wait_for_mount(Mount, 0);
                {ok, Pico} ->
                    {ok, Pico}
            end;
        Path ->
            case file:read_link_info(Path) of
                {ok, #file_info{type = directory} = _Info} ->
                    rebar_api:debug("Pico mounted at ~s.", [Mount]),
                    {ok, Path};
                _ ->
                    rebar_api:info("Waiting for the device at path ~s to mount...", [
                        string:trim(Mount)
                    ]),
                    wait_for_mount(Mount, 0)
            end
    end.

%% @private
needs_reset(ResetDev) ->
    case ResetDev of
        [] ->
            false;
        [ResetPort | _T] ->
            case file:read_link_info(ResetPort) of
                {ok, #file_info{type = device} = _Info} ->
                    {true, ResetPort};
                {ok, #file_info{type = symlink} = _Info} ->
                    {true, ResetPort};
                _ ->
                    false
            end;
        _ ->
            false
    end.

%% @private
do_reset(ResetPort) ->
    Flag = get_stty_file_flag(),
    BootselMode = lists:join(" ", [
        "stty", Flag, ResetPort, "1200"
    ]),
    rebar_api:info("Resetting device at path ~s", [ResetPort]),
    ResetStatus = os:cmd(BootselMode),
    case ResetStatus of
        "" ->
            ok;
        Error ->
            case os:find_executable(picotool) of
                false ->
                    rebar_api:error(
                        "Warning: ~s~nUnable to locate 'picotool', close the serial monitor before flashing, or install picotool for automatic disconnect and BOOTSEL mode.",
                        [Error]
                    ),
                    erlang:throw(reset_error);
                Picotool ->
                    rebar_api:warn(
                        "Warning: ~s~nFor faster flashing remember to disconnect serial monitor first.",
                        [Error]
                    ),
                    DevReset = lists:join(" ", [
                        Picotool, "reboot", "-f", "-u"
                    ]),
                    rebar_api:warn("Disconnecting serial monitor with: `~s' in 5 seconds...", [
                        DevReset
                    ]),
                    timer:sleep(5000),
                    RebootReturn = os:cmd(DevReset),
                    RebootStatus = string:trim(RebootReturn),
                    case RebootStatus of
                        "The device was asked to reboot into BOOTSEL mode." ->
                            ok;
                        BootError ->
                            rebar_api:error("Failed to prepare pico for flashing: ~s", [BootError]),
                            erlang:throw(picoflash_reboot_error)
                    end
            end
    end.

%% @private
get_uf2_file(ProjectApps) ->
    [App | _] = [ProjectApp || ProjectApp <- ProjectApps],
    OutDir = rebar_app_info:out_dir(App),
    Name = binary_to_list(rebar_app_info:name(App)),
    DirName = filename:dirname(OutDir),
    filename:join(DirName, Name ++ ".uf2").

%% @private
get_uf2_appname(ProjectApps) ->
    [App | _] = [ProjectApp || ProjectApp <- ProjectApps],
    binary_to_list(rebar_app_info:name(App)).

%% @private
do_flash(ProjectApps, PicoPath, ResetDev) ->
    case needs_reset(ResetDev) of
        false ->
            rebar_api:debug("No Pico reset device found matching ~s.", [ResetDev]),
            ok;
        {true, ResetPort} ->
            rebar_api:debug("Pico at ~s needs reset...", [ResetPort]),
            do_reset(ResetPort),
            rebar_api:info("Waiting for the device at path ~s to settle and mount...", [PicoPath]),
            wait_for_mount(PicoPath, 0)
    end,
    {ok, Path} = get_pico_mount(PicoPath),
    TargetUF2 = get_uf2_file(ProjectApps),
    App = get_uf2_appname(ProjectApps),
    File = App ++ ".uf2",
    Dest = filename:join(Path, File),
    rebar_api:info("Copying ~s to ~s", [File, Path]),
    case file:copy(TargetUF2, Dest) of
        {ok, _Size} ->
            ok;
        CopyError ->
            rebar_api:error("Failed to copy application file ~s to pico: ~s", [File, CopyError]),
            erlang:throw(picoflash_copy_error)
    end,
    rebar_api:info("Successfully loaded ~s application to the device.", [App]),
    ok.
