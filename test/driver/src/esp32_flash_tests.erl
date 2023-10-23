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
-module(esp32_flash_tests).

-export([run/1]).

run(Opts) ->
    ok = test_flags(Opts),
    ok = test_env_overrides(Opts),
    ok = test_rebar_overrides(Opts),
    ok.

%% @private
test_flags(Opts) ->

    test_flags(Opts, [], [
        {"--chip", "auto"},
        {"--port", "/dev/ttyUSB0"},
        {"--baud", "115200"},
        {"--offset", "0x210000"}
    ]),

    test_flags(Opts, [
            {"-c", "esp32c3"},
            {"-p", "/dev/tty.usbserial-0001"}
        ], [
            {"--chip", "esp32c3"},
            {"--port", "tty.usbserial-0001"},
            {"--baud", "115200"},
            {"--offset", "0x210000"}
    ]),

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
    ok = test:expect_contains("_build/default/lib/myapp.avm", Output),

    test:tick().

%% @private
test_env_overrides(Opts) ->
    test_env_overrides(Opts, "ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_PORT", "/dev/tty.usbserial-0001", "--port"),
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
    test_rebar_overrides(Opts, [], "ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_CHIP", "esp32", "--chip", "esp32c3"),
    test_rebar_overrides(Opts, [{"-c", "esp32h2"}], "ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_CHIP", "esp32", "--chip", "esp32h2"),
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
create_esp32_flash_cmd(AppDir, Opts, Env) ->
    test:create_rebar3_cmd(AppDir, esp32_flash, [{"-e", "echo"} | Opts], Env).
