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
            "Use this plugin to convert an AtomVM packbeam file to a uf2 file and copy to an rp2040 device.~n"
        }
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
            get_default_mount()
        ),
        reset => os:getenv(
            "ATOMVM_REBAR3_PLUGIN_PICO_RESET_DEV",
            get_reset_dev()
        )
    }.

%% @private
get_stty_file_flag() ->
    System = os:cmd("uname -s"),
    case System of
        "Linux\n" ->
            "-F";
        _Other ->
            "-f"
    end.

%% @private
get_reset_dev() ->
    System = os:cmd("uname -s"),
    case System of
        "Linux\n" ->
            "/dev/ttyACM0";
        "Darwin\n" ->
            "/dev/cu.usbmodem14*";
        _Other ->
            ""
    end.

%% @private
get_default_mount() ->
    System = os:cmd("uname -s"),
    case System of
        "Linux\n" ->
            "/run/media/${USER}/RPI-RP2";
        "Darwin\n" ->
            "/Volumes/RPI-RP2";
        _Other ->
            ""
    end.

%% @private
wait_for_mount(Mount, Count) when Count < 30 ->
    Cmd = lists:join(" ", [
        "sh -c \"(test -d", Mount, "&& echo 'true') || echo 'false'\""
    ]),
    case os:cmd(Cmd) of
        "true\n" ->
            ok;
        "false\n" ->
            timer:sleep(1000),
            wait_for_mount(Mount, Count + 1)
    end;
wait_for_mount(Mount, 30) ->
    rebar_api:error("Pico not mounted at ~s after 30 seconds. giving up...", [Mount]),
    erlang:throw(mount_error).

%% @private
check_pico_mount(Mount) ->
    Cmd = lists:join(" ", [
        "sh -c \"(test -d", Mount, "&& echo 'true') || echo 'false'\""
    ]),
    case os:cmd(Cmd) of
        "true\n" ->
            ok;
        "false\n" ->
            rebar_api:error("Pico not mounted at ~s.", [Mount]),
            erlang:throw(no_device)
    end.

%% @private
needs_reset(ResetPort) ->
    Test = lists:join(" ", [
        "sh -c \"(test -e",
        ResetPort,
        "&& echo 'true') || echo 'false'\""
    ]),
    case os:cmd(Test) of
        "true\n" ->
            true;
        "false\n" ->
            false
    end.

%% @private
get_uf2_file(ProjectApps) ->
    [App | _] = [ProjectApp || ProjectApp <- ProjectApps],
    OutDir = rebar_app_info:out_dir(App),
    Name = binary_to_list(rebar_app_info:name(App)),
    DirName = filename:dirname(OutDir),
    filename:join(DirName, Name ++ ".uf2").

%% @private
do_flash(ProjectApps, PicoPath, ResetPort) ->
    case needs_reset(ResetPort) of
        false ->
            ok;
        true ->
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
                            rebar_api:error("Warning: ~s~nUnable to locate 'picotool', close the serial monitor before flashing, or install picotool for automatic disconnect and BOOTSEL mode.", [Error]),
                            erlang:throw(reset_error);
                        _Path ->
                            rebar_api:warn("Warning: ~s~nFor faster flashing remember to disconnect serial monitor first.", [Error]),
                            DevReset = lists:join(" ", [
                                "picotool", "reboot", "-f", "-u"
                            ]),
                            rebar_api:warn("Disconnecting serial monitor with: `~s' in 5 seconds...", [DevReset]),
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
            end,
            PrettyPath = lists:join(" ", [
                "echo", PicoPath
            ]),
            rebar_api:info("Waiting for the device at path ~s to settle and mount...", [string:trim(os:cmd(PrettyPath))]),
            wait_for_mount(PicoPath, 0)
    end,
    check_pico_mount(PicoPath),
    TargetUF2 = get_uf2_file(ProjectApps),
    Cmd = lists:join(" ", [
        "cp", "-v", TargetUF2, PicoPath
    ]),
    rebar_api:info("Copying packbeam files...~n~s", [os:cmd(Cmd)]),
    ok.
