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
-module(packbeam_tests).

-export([run/1]).

run(Opts) ->
    ok = test_defaults(Opts),
    ok = test_start(Opts),
    ok = test_prune(Opts),
    ok = test_rebar_overrides(Opts),
    ok = test_otp_application(Opts),
    ok.

%% @private
test_defaults(Opts) ->
    AppsDir = maps:get(apps_dir, Opts),
    AppDir = test:make_path([AppsDir, "myapp"]),

    %% -f temporary during dev
    Cmd = create_packbeam_cmd(AppDir, ["-f"], []),
    Output = test:execute_cmd(Cmd, Opts),
    test:debug(Output, Opts),

    ok = test:expect_contains("AVM file written to", Output),
    ok = test:expect_contains("_build/default/lib/myapp.avm", Output),
    AVMPath = test:make_path([AppDir, "_build/default/lib/myapp.avm"]),
    ok = test:file_exists(AVMPath),
    AVMElements = test:get_avm_elements(AVMPath),

    2 = length(AVMElements),
    [MyAppBeam, MyAppApplicationBin] = AVMElements,

    true = packbeam_api:is_beam(MyAppBeam),
    true = packbeam_api:is_entrypoint(MyAppBeam),

    false = packbeam_api:is_beam(MyAppApplicationBin),
    false = packbeam_api:is_entrypoint(MyAppApplicationBin),

    test:tick().

%% @private
test_start(Opts) ->
    AppsDir = maps:get(apps_dir, Opts),
    AppDir = test:make_path([AppsDir, "multi-start"]),

    %% -f temporary during dev
    Cmd = create_packbeam_cmd(AppDir, ["-f"], []),
    Output = test:execute_cmd(Cmd, Opts),
    test:debug(Output, Opts),

    ok = test:expect_contains("AVM file written to", Output),
    ok = test:expect_contains("_build/default/lib/myapp.avm", Output),
    AVMPath = test:make_path([AppDir, "_build/default/lib/myapp.avm"]),
    ok = test:file_exists(AVMPath),
    AVMElements = test:get_avm_elements(AVMPath),

    3 = length(AVMElements),
    [BeamA, BeamB, MyAppApplicationBin] = AVMElements,

    % Both BeamA and BeamB contain start/0 and are currently moved first
    % by atomvm_packbeam_provider. Which one is sorted before the other is not
    % defined.
    {MyAppBeam, StartBeam} =
        case {BeamA, BeamB} of
            {[{module, myapp} | _], [{module, start} | _]} -> {BeamA, BeamB};
            {[{module, start} | _], [{module, myapp} | _]} -> {BeamB, BeamA}
        end,

    {module, myapp} = hd(MyAppBeam),
    true = packbeam_api:is_beam(MyAppBeam),
    true = packbeam_api:is_entrypoint(MyAppBeam),

    {module, start} = hd(StartBeam),
    true = packbeam_api:is_beam(StartBeam),
    true = packbeam_api:is_entrypoint(StartBeam),

    false = packbeam_api:is_beam(MyAppApplicationBin),
    false = packbeam_api:is_entrypoint(MyAppApplicationBin),

    %%
    %% Now specify `-s start` to get the start module first
    %% In this case, packbeam will only make one beam an entrypoint
    %%

    Cmd2 = create_packbeam_cmd(AppDir, ["-f", {"-s", "start"}], []),
    _Output2 = test:execute_cmd(Cmd2, Opts),
    AVMElements2 = test:get_avm_elements(AVMPath),

    3 = length(AVMElements2),
    [StartBeam1, MyAppBeam1, MyAppApplicationBin1] = AVMElements2,

    {module, start} = hd(StartBeam1),
    true = packbeam_api:is_beam(StartBeam1),
    true = packbeam_api:is_entrypoint(StartBeam1),
    StartBeam1 = StartBeam,

    {module, myapp} = hd(MyAppBeam1),
    true = packbeam_api:is_beam(MyAppBeam1),
    % Whether the second beam is an entrypoint is not verified by
    % packbeam tests and becomes true with 0.8.0
    % false = packbeam_api:is_entrypoint(MyAppBeam1),

    false = packbeam_api:is_beam(MyAppApplicationBin1),
    false = packbeam_api:is_entrypoint(MyAppApplicationBin1),
    MyAppApplicationBin1 = MyAppApplicationBin,

    test:tick().

%% @private
test_prune(Opts) ->
    AppsDir = maps:get(apps_dir, Opts),
    AppDir = test:make_path([AppsDir, "prune"]),

    %% -f temporary during dev
    Cmd = create_packbeam_cmd(AppDir, ["-f"], []),
    Output = test:execute_cmd(Cmd, Opts),
    test:debug(Output, Opts),

    ok = test:expect_contains("AVM file written to", Output),
    ok = test:expect_contains("_build/default/lib/myapp.avm", Output),
    AVMPath = test:make_path([AppDir, "_build/default/lib/myapp.avm"]),
    ok = test:file_exists(AVMPath),
    AVMElements = test:get_avm_elements(AVMPath),

    6 = length(AVMElements),

    {value, ABeam} = test:find_avm_element_by_name("a.beam", AVMElements),
    {value, BBeam} = test:find_avm_element_by_name("b.beam", AVMElements),
    {value, CBeam} = test:find_avm_element_by_name("c.beam", AVMElements),
    {value, DBeam} = test:find_avm_element_by_name("d.beam", AVMElements),

    true = packbeam_api:is_beam(ABeam),
    true = packbeam_api:is_beam(BBeam),
    true = packbeam_api:is_beam(CBeam),
    true = packbeam_api:is_beam(DBeam),

    %%
    %% Now specify `-p` to prune out d.beam, since no one references him
    %%

    Cmd2 = create_packbeam_cmd(AppDir, ["-f", "-p"], []),
    _Output2 = test:execute_cmd(Cmd2, Opts),
    AVMElements2 = test:get_avm_elements(AVMPath),

    5 = length(AVMElements2),
    {value, ABeam} = test:find_avm_element_by_name("a.beam", AVMElements2),
    {value, BBeam} = test:find_avm_element_by_name("b.beam", AVMElements2),
    {value, CBeam} = test:find_avm_element_by_name("c.beam", AVMElements2),
    false = test:find_avm_element_by_name("d.beam", AVMElements2),

    test:tick().

%% @private
test_rebar_overrides(Opts) ->
    AppsDir = maps:get(apps_dir, Opts),
    AppDir = test:make_path([AppsDir, "rebar_overrides"]),

    %% -f temporary during dev
    Cmd = create_packbeam_cmd(AppDir, ["-f"], []),
    Output = test:execute_cmd(Cmd, Opts),
    test:debug(Output, Opts),

    ok = test:expect_contains("AVM file written to", Output),
    ok = test:expect_contains("_build/default/lib/myapp.avm", Output),
    AVMPath = test:make_path([AppDir, "_build/default/lib/myapp.avm"]),
    ok = test:file_exists(AVMPath),
    AVMElements = test:get_avm_elements(AVMPath),

    3 = length(AVMElements),
    [BeamA, BeamB, MyAppApplicationBin] = AVMElements,

    % Both BeamA and BeamB contain start/0 and are currently moved first
    % by atomvm_packbeam_provider. Which one is sorted before the other is not
    % defined.
    {MyAppBeam, StartBeam} =
        case {BeamA, BeamB} of
            {[{module, myapp} | _], [{module, start} | _]} -> {BeamA, BeamB};
            {[{module, start} | _], [{module, myapp} | _]} -> {BeamB, BeamA}
        end,

    {module, start} = hd(StartBeam),
    true = packbeam_api:is_beam(StartBeam),
    true = packbeam_api:is_entrypoint(StartBeam),

    {module, myapp} = hd(MyAppBeam),
    true = packbeam_api:is_beam(MyAppBeam),
    % Whether the second beam is not an entrypoint is not verified by
    % packbeam tests and becomes true with 0.8.0
    % false = packbeam_api:is_entrypoint(MyAppBeam),

    false = packbeam_api:is_beam(MyAppApplicationBin),
    false = packbeam_api:is_entrypoint(MyAppApplicationBin),

    %%
    %% Now specify `-s myapp` to get the myapp module first
    %%

    Cmd2 = create_packbeam_cmd(AppDir, ["-f", {"-s", "myapp"}], []),
    _Output2 = test:execute_cmd(Cmd2, Opts),
    AVMElements2 = test:get_avm_elements(AVMPath),

    3 = length(AVMElements2),
    [MyAppBeam1, StartBeam1, MyAppApplicationBin1] = AVMElements2,

    true = packbeam_api:is_beam(MyAppBeam1),
    true = packbeam_api:is_entrypoint(MyAppBeam1),

    true = packbeam_api:is_beam(StartBeam1),
    % Whether the second beam is not an entrypoint is not verified by
    % packbeam tests and becomes true with 0.8.0
    % false = packbeam_api:is_entrypoint(StartBeam1),

    false = packbeam_api:is_beam(MyAppApplicationBin1),
    false = packbeam_api:is_entrypoint(MyAppApplicationBin1),

    test:tick().

%% @private
test_otp_application(Opts) ->
    AppsDir = maps:get(apps_dir, Opts),
    AppDir = test:make_path([AppsDir, "otp_application"]),

    %% -f temporary during dev
    Cmd = create_packbeam_cmd(AppDir, ["-f"], []),
    Output = test:execute_cmd(Cmd, Opts),
    test:debug(Output, Opts),

    ok = test:expect_contains("AVM file written to", Output),
    ok = test:expect_contains("_build/default/lib/my_app.avm", Output),
    AVMPath = test:make_path([AppDir, "_build/default/lib/my_app.avm"]),
    ok = test:file_exists(AVMPath),
    AVMElements = test:get_avm_elements(AVMPath),

    [InitShimBeam | _Rest] = AVMElements,
    true = packbeam_api:is_beam(InitShimBeam),
    true = packbeam_api:is_entrypoint(InitShimBeam),

    {value, StartBoot} = test:find_avm_element_by_name("init/priv/start.boot", AVMElements),
    false = packbeam_api:is_beam(StartBoot),

    {value, MyAppBeam} = test:find_avm_element_by_name("my_app.beam", AVMElements),
    true = packbeam_api:is_beam(MyAppBeam),
    false = packbeam_api:is_entrypoint(MyAppBeam),

    {value, MyAppApplicationBin} = test:find_avm_element_by_name(
        "my_app/priv/application.bin", AVMElements
    ),
    false = packbeam_api:is_beam(MyAppApplicationBin),
    false = packbeam_api:is_entrypoint(MyAppApplicationBin),

    test:tick().

%% @private
create_packbeam_cmd(AppDir, Opts, Env) ->
    test:create_rebar3_cmd(AppDir, packbeam, Opts, Env).
