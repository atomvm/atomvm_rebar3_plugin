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
-module(esp32_flash_tests).

-export([run/1]).

run(Opts) ->
    ok = test_flags(Opts),
    ok = test_elixir_partition_table(Opts),
    ok = test_env_overrides(Opts),
    ok = test_rebar_overrides(Opts),
    ok = test_errors(Opts),
    ok.

%% @private
test_flags(Opts) ->
    test_flags(Opts, [], [
        {"--chip", "auto"},
        {"--baud", "115200"},
        {"--offset", "0x210000"}
    ]),

    test_flags(
        Opts,
        [
            {"-c", "esp32c3"},
            {"-p", "/dev/tty.usbserial-0001"},
            {"-b", "921600"}
        ],
        [
            {"--chip", "esp32c3"},
            {"--port", "/dev/tty.usbserial-0001"},
            {"--baud", "921600"},
            {"--offset", "0x210000"}
        ]
    ),

    test_flags(
        Opts,
        [
            {"-a", "app1.avm"}
        ],
        [
            {"--chip", "auto"},
            {"--baud", "115200"},
            {"--offset", "0x310000"}
        ]
    ),
    ok.

%% @private
test_flags(Opts, Flags, FlagExpectList) ->
    AppsDir = maps:get(apps_dir, Opts),
    AppDir = test:make_path([AppsDir, "myapp"]),

    Cmd = create_esp32_flash_cmd(AppDir, Flags, []),
    Output = test:execute_cmd(Cmd, Opts),
    test:debug(Output, Opts),

    lists:foreach(
        fun({Flag, Value}) ->
            test:expect_contains(io_lib:format("~s ~s", [Flag, Value]), Output)
        end,
        FlagExpectList
    ),
    ok = test:expect_contains("Flashing myapp.avm to device.", Output),

    test:tick().

test_elixir_partition_table(Opts) ->
    AppsDir = maps:get(apps_dir, Opts),
    AppDir = test:make_path([AppsDir, "myapp"]),
    Offset = 16#250000,

    Cmd = create_esp32_flash_cmd(AppDir, [], [
        {"ATOMVM_REBAR3_PLUGIN_PARTITION_DATA",
            os:getenv("ATOMVM_REBAR3_PLUGIN_ESP32_EX_PARTITION_DUMP")}
    ]),
    Output = test:execute_cmd(Cmd, Opts),
    test:debug(Output, Opts),

    ok = test:expect_contains(io_lib:format("~i", [Offset]), Output),

    test:tick().

%% @private
test_env_overrides(Opts) ->
    test_env_overrides(
        Opts, "ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_PORT", "/dev/tty.usbserial-0001", "--port"
    ),
    test_env_overrides(Opts, "ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_CHIP", "esp32", "--chip"),
    test_env_overrides(Opts, "ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_BAUD", "921600", "--baud"),
    test_env_overrides(Opts, "ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_OFFSET", "0x1000", ""),
    ok.

%% @private
test_env_overrides(Opts, EnvVar, Value, Flag) ->
    AppsDir = maps:get(apps_dir, Opts),
    AppDir = test:make_path([AppsDir, "myapp"]),

    Cmd = create_esp32_flash_cmd(AppDir, [], [{EnvVar, Value}]),
    Output = test:execute_cmd(Cmd, Opts),
    test:debug(Output, Opts),

    ok = test:expect_contains(io_lib:format("~s ~s", [Flag, Value]), Output),

    test:tick().

%% @private
test_rebar_overrides(Opts) ->
    %% the rebar_overrides rebar.config specifies tge esp32c3 chip
    test_rebar_overrides(
        Opts, [], "ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_CHIP", "esp32", "--chip", "esp32c3"
    ),
    test_rebar_overrides(
        Opts,
        [{"-c", "esp32h2"}],
        "ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_CHIP",
        "esp32",
        "--chip",
        "esp32h2"
    ),
    ok.

%% @private
test_rebar_overrides(Opts, Flags, EnvVar, Value, Flag, ExpectedValue) ->
    AppsDir = maps:get(apps_dir, Opts),
    AppDir = test:make_path([AppsDir, "rebar_overrides"]),

    Cmd = create_esp32_flash_cmd(AppDir, Flags, [{EnvVar, Value}]),
    Output = test:execute_cmd(Cmd, Opts),
    test:debug(Output, Opts),

    ok = test:expect_contains(io_lib:format("~s ~s", [Flag, ExpectedValue]), Output),

    test:tick().

%% @private
test_errors(Opts) ->
    test_errors(
        Opts,
        [
            {"-a", "fake"}
        ],
        "The partition fake was not fount on device partition table!"
    ),

    %% overrides the script that emulates dumping the partition data
    %% simulating execution when no device is attached.
    test_errors(
        Opts,
        [
            {"--esptool", "echo"}
        ],
        "No ESP32 device attached!"
    ),

    test_errors(
        Opts,
        [
            {"--app_partition", "app1.avm"},
            {"--offset", "0x210000"}
        ],
        "The configured offset 0x210000 does not match the partition table on the device (0x310000)."
    ),

    test_errors(
        Opts,
        [
            {"-a", "bad1"}
        ],
        "The partition bad1 was found, but used invalid subtype 0x06."
    ),

    test_errors(
        Opts,
        [
            {"-a", "bad2"}
        ],
        "The partition bad2 was found, but partition data is invalid."
    ),

    ok.

%% @private
test_errors(Opts, Flags, Expect) ->
    AppsDir = maps:get(apps_dir, Opts),
    AppDir = test:make_path([AppsDir, "myapp"]),

    Cmd = create_esp32_flash_cmd(AppDir, Flags, []),
    Output = test:execute_cmd(Cmd, Opts),
    test:debug(Output, Opts),

    ok = test:expect_contains(Expect, Output),

    test:tick().

%% @private
create_esp32_flash_cmd(AppDir, Opts, Env) ->
    test:create_rebar3_cmd(AppDir, esp32_flash, Opts, Env).
