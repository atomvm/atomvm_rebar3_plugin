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
-module(test).

-export([run/1]).

-export([
    create_rebar3_cmd/4,
    debug/2,
    execute_cmd/1,
    execute_cmd/2,
    expect_contains/2,
    expect_equal/2,
    file_exists/1,
    find_avm_element_by_name/2,
    get_avm_elements/1,
    make_path/1,
    tick/0
]).

run(Opts) ->
    try
        prepare_tests(),
        run_tests(Opts)
    catch
        C:E:S ->
            {{class, C}, {error, E}, {stacktrace, S}}
    end.

%% @private
prepare_tests() ->
    expect_equal("done", string:strip(os:cmd("scripts/prepare.sh"), both)),
    ok.

%% @private
run_tests(Opts) ->

    io:put_chars("packbeam_tests: "),
    ok = packbeam_tests:run(Opts),
    io:put_chars("\n"),

    io:put_chars("esp32_flash_tests: "),
    ok = esp32_flash_tests:run(Opts),
    io:put_chars("\n"),

    io:put_chars("stm32_flash_tests: "),
    ok = stm32_flash_tests:run(Opts),
    io:put_chars("\n"),

    io:put_chars("bootstrap_tests: "),
    ok = bootstrap_tests:run(Opts),
    io:put_chars("\n"),

    ok.

make_path(Elements) ->
    lists:flatten(lists:join("/", Elements)).

tick() ->
    io:put_chars(".").

expect_contains(String, Output) ->
    case string:find(Output, String) of
        nomatch ->
            {error, {expected, String, contained_in, Output}};
        _ ->
            ok
    end.

expect_equal(String, Output) ->
    case string:equal(Output, String) of
        false ->
            {error, {expected, String, equal_to, Output}};
        _ ->
            ok
    end.

file_exists(Path) ->
    case filelib:is_regular(Path) of
        true ->
            ok;
        _ ->
            {error, {expected, Path, to_exist}}
    end.

create_rebar3_cmd(AppPath, Task, Opts, Env) ->
    io_lib:format("cd ~s && ~s rebar3 atomvm ~p ~s", [AppPath, make_env(Env), Task, make_opts(Opts)]).

make_env(Env) ->
    lists:foldl(
        fun({Key, Value}, Accum) ->
            io_lib:format("~s=~s ", [Key, Value]) ++ Accum
        end,
        [],
        Env
     ).

make_opts(Opts) ->
    lists:foldl(
        fun({Key, Value}, Accum) ->
            io_lib:format("~s ~s ", [Key, Value]) ++ Accum;
           (Key, Accum) ->
            io_lib:format("~s ", [Key]) ++ Accum
        end,
        [],
        Opts
     ).

execute_cmd(Cmd) ->
    execute_cmd(Cmd, false).

execute_cmd(Cmd, Opts) ->
    case maps:get(verbose, Opts) orelse maps:get(debug, Opts) of
        true ->
            io:format("#executing> ~s~n", [Cmd]);
        _ ->
            ok
    end,
    %% TODO make this a port
    os:cmd(Cmd).

debug(Msg, Opts) ->
    case maps:get(debug, Opts) of
        true ->
            io:format("~s~n", [Msg]);
        _ ->
            ok
    end.

get_avm_elements(AVMPath) ->
    packbeam_api:list(AVMPath).

find_avm_element_by_name(AVMElementName, AVMElements) ->
    lists:search(
        fun(AVMElement) ->
            string:equal(
                packbeam_api:get_element_name(AVMElement),
                AVMElementName
            )
        end,
        AVMElements
    ).
