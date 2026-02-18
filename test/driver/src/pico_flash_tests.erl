%%
%% Copyright (c) 2023 <fred@dushin.net>
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
-module(pico_flash_tests).

-export([run/1]).

run(Opts) ->
    ok = test_flags(Opts),
    ok = test_env_overrides(Opts),
    ok = test_rebar_overrides(Opts),
    ok.

%% @private
test_flags(Opts) ->
    %% test default options, and no device connected (this test wiill fail if a real pico is connected!)
    test_flags(Opts, [], [
        {"--path\n", ""},
        {"--reset\n", ""},
        {"Pico not mounted after 30 seconds.", "giving up..."}
    ]),

    %% Simulate a device that needs reset
    Reset1 = os:getenv("TEST_MYAPP_LOCK"),
    %% devices needing reset can only be device files or symlinks
    file:make_symlink("/dev/null", Reset1),
    Path1 = os:getenv("TEST_MYAPP_MOUNT"),
    %% make sure the mount is not present (picotool.sh will create the mount after removing lock dev)
    file:del_dir_r("TEST_MYAPP_MOUNT"),
    test_flags(
        Opts,
        [
            {"-r", Reset1},
            {"-p", Path1}
        ],
        [
            {"--path", Path1},
            {"--reset", Reset1},
            {
                "Copying myapp.uf2 to " ++ Path1,
                "===> Successfully loaded myapp application to the device."
            }
        ]
    ),

    ok.

%% @private
test_flags(Opts, Flags, FlagExpectList) ->
    AppsDir = maps:get(apps_dir, Opts),
    AppDir = test:make_path([AppsDir, "myapp"]),

    Cmd = create_pico_flash_cmd(AppDir, Flags, []),
    Output = test:execute_cmd(Cmd, Opts),
    test:debug(Output, Opts),

    lists:foreach(
        fun({Flag, Value}) ->
            test:expect_contains(io_lib:format("~s ~s", [Flag, Value]), Output)
        end,
        FlagExpectList
    ),

    test:tick().

%% @private
test_env_overrides(Opts) ->
    Reset = os:getenv("TEST_MYAPP_LOCK"),
    file:make_symlink("/dev/null", Reset),
    test_env_overrides(
        Opts, "ATOMVM_REBAR3_PLUGIN_PICO_RESET_DEV", Reset, "--reset"
    ),

    Path = os:getenv("TEST_MYAPP_MOUNT"),
    %% there is no dev lock, so we must make sure the mount exists and is empty
    file:del_dir_r(Path),
    file:make_dir(Path),
    test_env_overrides(Opts, "ATOMVM_REBAR3_PLUGIN_PICO_MOUNT_PATH", Path, "--path"),
    %% cleanup
    file:del_dir_r(Path),

    file:make_dir(Path),
    test_env_overrides(
        Opts, "ATOMVM_REBAR3_PLUGIN_PICOTOOL", string:trim(os:cmd("which echo")), "--picotool"
    ),
    %% cleanup
    file:del_dir_r(Path),
    ok.

%% @private
test_env_overrides(Opts, EnvVar, Value, Flag) ->
    AppsDir = maps:get(apps_dir, Opts),
    AppDir = test:make_path([AppsDir, "myapp"]),
    %% if we are not testing path overrides use the test path since no device is present
    Flags =
        case Flag of
            "--path" ->
                [];
            _ ->
                [{"-p", os:getenv("TEST_MYAPP_MOUNT")}]
        end,
    Cmd = create_pico_flash_cmd(AppDir, Flags, [{EnvVar, Value}]),
    Output = test:execute_cmd(Cmd, Opts),
    test:debug(Output, Opts),

    ok = test:expect_contains(io_lib:format("~s ~s", [Flag, Value]), Output),

    test:tick().

%% @private
test_rebar_overrides(Opts) ->
    %% the rebar_overrides rebar.config specifies reset /dev/FAKE0
    Path = os:getenv("TEST_REBAR_OVERRIDES_MOUNT"),
    file:del_dir_r(Path),
    file:make_dir(Path),
    test_rebar_overrides(
        Opts,
        [{"-p", Path}],
        "ATOMVM_REBAR3_PLUGIN_PICO_RESET_DEV",
        "/dev/ttyACM0",
        "--reset",
        "/dev/FAKE0"
    ),
    %% cleanup
    file:del_dir_r(Path),

    %% Simulate a device needing reset, the mock picotool.sh will create the mount matching the reset device.
    Reset = os:getenv("TEST_REBAR_OVERRIDES_LOCK"),
    file:make_symlink("/dev/null", Reset),
    test_rebar_overrides(
        Opts,
        [{"-r", Reset}, {"-p", Path}],
        "ATOMVM_REBAR3_PLUGIN_PICO_RESET_DEV",
        "/dev/tty.usbserial-0001",
        "--reset",
        Reset
    ),
    %% cleanup
    file:del_dir_r(Path),

    %% Simulte a device already in BOOTSEL mode
    file:make_dir(Path),
    test_rebar_overrides(
        Opts,
        [{"-p", Path}],
        "ATOMVM_REBAR3_PLUGIN_PICO_MOUNT_PATH",
        "/mnt/RP2350",
        "--path",
        Path
    ),
    ok.

%% @private
test_rebar_overrides(Opts, Flags, EnvVar, Value, Flag, ExpectedValue) ->
    AppsDir = maps:get(apps_dir, Opts),
    AppDir = test:make_path([AppsDir, "rebar_overrides"]),

    Cmd = create_pico_flash_cmd(AppDir, Flags, [{EnvVar, Value}]),
    Output = test:execute_cmd(Cmd, Opts),
    test:debug(Output, Opts),

    ok = test:expect_contains(io_lib:format("~s ~s", [Flag, ExpectedValue]), Output),

    test:tick().

%% @private
create_pico_flash_cmd(AppDir, Opts, Env) ->
    test:create_rebar3_cmd(AppDir, pico_flash, Opts, Env).
