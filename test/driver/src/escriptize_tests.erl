%%
%% Copyright (c) 2025 Paul Guyot <pguyot@kallisys.net>
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
-module(escriptize_tests).

-export([run/1]).

run(Opts) ->
    ok = test_defaults(Opts),
    ok.

%% @private
test_defaults(Opts) ->
    AppsDir = maps:get(apps_dir, Opts),
    AppDir = test:make_path([AppsDir, "myscript"]),

    Cmd = create_escriptize_cmd(AppDir, [], []),
    Output = test:execute_cmd(Cmd, Opts),
    test:debug(Output, Opts),

    ok = test:expect_contains("Created packed AVM:", Output),
    ok = test:expect_contains("_build/default/lib/myscript_packed.avm", Output),
    ok = test:expect_contains("with start module myscript", Output),

    ok = test:expect_contains("Created standalone executable:", Output),
    ok = test:expect_contains("_build/default/bin/myscript", Output),

    ExecPath = test:make_path([AppDir, "_build/default/bin/myscript"]),
    ok = test:file_exists(ExecPath),

    [] = test:execute_cmd(ExecPath),

    test:tick().

%% @private
create_escriptize_cmd(AppDir, Opts, Env) ->
    test:create_rebar3_cmd(AppDir, escriptize, Opts, Env).
