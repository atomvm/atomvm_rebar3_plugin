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
-module(bootstrap_tests).

-export([run/1]).

run(Opts) ->
    ok = test_bootstrap_task(Opts),
    ok = test_packbeam_with_bootstrap(Opts),
    ok.

%% @private
test_bootstrap_task(Opts) ->
    AppsDir = maps:get(apps_dir, Opts),
    AppDir = test:make_path([AppsDir, "bootstrap"]),

    Cmd = create_bootstrap_cmd(AppDir, ["-f"], []),
    Output = test:execute_cmd(Cmd, Opts),
    test:debug(Output, Opts),

    AppblicationBeamPath = test:make_path([
        AppDir, "_build/default/lib/myapp/bootstrap_ebin/application.beam"
    ]),
    ok = test:file_exists(AppblicationBeamPath),

    test:tick().

test_packbeam_with_bootstrap(Opts) ->
    AppsDir = maps:get(apps_dir, Opts),
    AppDir = test:make_path([AppsDir, "bootstrap"]),

    Cmd = create_packbeam_cmd(AppDir, ["-f"], []),
    Output = test:execute_cmd(Cmd, Opts),
    test:debug(Output, Opts),

    ok = test:expect_contains("AVM file written to", Output),
    ok = test:expect_contains("_build/default/lib/myapp.avm", Output),
    AVMPath = test:make_path([AppDir, "_build/default/lib/myapp.avm"]),
    ok = test:file_exists(AVMPath),
    AVMElements = test:get_avm_elements(AVMPath),

    4 = length(AVMElements),
    [_MyAppBeam, _StartBeam, ApplicationBeam, _MyAppApplicationBin] = AVMElements,

    true = packbeam_api:is_beam(ApplicationBeam),
    false = packbeam_api:is_entrypoint(ApplicationBeam),
    application = packbeam_api:get_element_module(ApplicationBeam),

    test:tick().

%% @private
create_bootstrap_cmd(AppDir, Opts, Env) ->
    test:create_rebar3_cmd(AppDir, bootstrap, Opts, Env).

%% @private
create_packbeam_cmd(AppDir, Opts, Env) ->
    test:create_rebar3_cmd(AppDir, packbeam, Opts, Env).
