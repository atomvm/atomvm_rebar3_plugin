%%
%% Copyright (c) 2025 Winford (UncleGrumpy) <winford@object.stream>
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
-module(uf2create_tests).

-export([run/1]).

run(Opts) ->
    ok = test_defaults(Opts),
    ok = test_flags(Opts),
    ok = test_rebar_overrides(Opts),
    ok.

%% @private
test_defaults(Opts) ->
    AppsDir = maps:get(apps_dir, Opts),
    AppDir = test:make_path([AppsDir, "myapp"]),
    UF2Path = test:make_path([AppDir, "_build/default/lib/myapp.uf2"]),
    case test:file_exists(UF2Path) of
        ok ->
            Del = lists:join(" ", ["rm -v", UF2Path]),
            os:cmd(Del);
        _ ->
            ok
    end,

    Cmd = create_uf2create_cmd(AppDir, [], []),
    Output = test:execute_cmd(Cmd, Opts),
    test:debug(Output, Opts),

    ok = test:expect_contains("UF2 file written to", Output),
    ok = test:expect_contains("_build/default/lib/myapp.uf2", Output),
    ok = test:file_exists(UF2Path),

    test:tick().

test_flags(Opts) ->
    test_flags(Opts, [], [
        {"--start", "0x10180000"},
        {"--family_id", "universal"},
        {"--output", "_build/default/lib/myapp.uf2"},
        {"--input", "_build/default/lib/myapp.avm"}
    ]),
    test_flags(Opts, [{"-s", "0x12345"}, {"-f", "rp2040"}], [
        {"--start", "0x12345"},
        {"--family_id", "rp2040"}
    ]),
    test_flags(Opts, [{"--family_id", "rp2350"}], [
        {"--family_id", "data"}
    ]),
    ok.

test_flags(Opts, Flags, FlagExpectList) ->
    AppsDir = maps:get(apps_dir, Opts),
    AppDir = test:make_path([AppsDir, "myapp"]),
    UF2Path = test:make_path([AppDir, "_build/default/lib/myapp.uf2"]),
    case test:file_exists(UF2Path) of
        ok ->
            Del = lists:join(" ", ["rm -v", UF2Path]),
            os:cmd(Del);
        _ ->
            ok
    end,

    Cmd = create_uf2create_cmd(AppDir, Flags, []),
    Output = test:execute_cmd(Cmd, Opts),
    test:debug(Output, Opts),

    lists:foreach(
        fun({Flag, Value}) ->
            test:expect_contains(io_lib:format("~s ~s", [Flag, Value]), Output)
        end,
        FlagExpectList
    ),
    ok = test:expect_contains("UF2 file written to", Output),
    ok = test:expect_contains("_build/default/lib/myapp.uf2", Output),
    ok = test:file_exists(UF2Path),

    test:tick().

%% @private
test_rebar_overrides(Opts) ->
    %% the rebar_overrides rebar.config specifies start address "0x10180800"
    test_rebar_overrides(
        Opts, [], "ATOMVM_PICO_APP_START", "0xDEADBEEF", "--start", "0x10180800"
    ),

    test_rebar_overrides(
        Opts, [{"-s", "0x123456"}], "ATOMVM_PICO_APP_START", "0xDEADBEEF", "--start", "0x123456"
    ),

    %% the rebar_overrides rebar.config specifies family_id "rp2350" which is 'data'
    test_rebar_overrides(
        Opts, [], "ATOMVM_PICO_UF2_FAMILY", "rp2040", "--family_id", "data"
    ),

    test_rebar_overrides(
        Opts, [{"-f", "universal"}], "ATOMVM_PICO_UF2_FAMILY", "rp2040", "--family_id", "universal"
    ),

    ok.

%% @private
test_rebar_overrides(Opts, Flags, EnvVar, Value, Flag, ExpectedValue) ->
    AppsDir = maps:get(apps_dir, Opts),
    AppDir = test:make_path([AppsDir, "rebar_overrides"]),
    UF2Path = test:make_path([AppDir, "_build/default/lib/myapp.uf2"]),
    case test:file_exists(UF2Path) of
        ok ->
            Del = lists:join(" ", ["rm -v", UF2Path]),
            os:cmd(Del);
        _ ->
            ok
    end,

    Cmd = create_uf2create_cmd(AppDir, Flags, [{EnvVar, Value}]),
    Output = test:execute_cmd(Cmd, Opts),
    test:debug(Output, Opts),

    ok = test:expect_contains(io_lib:format("~s ~s", [Flag, ExpectedValue]), Output),
    ok = test:expect_contains("UF2 file written to", Output),
    ok = test:expect_contains("_build/default/lib/myapp.uf2", Output),
    ok = test:file_exists(UF2Path),

    test:tick().

%% @private
create_uf2create_cmd(AppDir, Opts, Env) ->
    test:create_rebar3_cmd(AppDir, uf2create, Opts, Env).
