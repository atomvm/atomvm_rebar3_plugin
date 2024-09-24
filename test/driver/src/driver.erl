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
-module(driver).

-export([main/1]).

-define(DEFAULT_OPTS, #{
    root => ".",
    verbose => false,
    debug => false
}).

main(Args) ->
    {Opts, _PositionalArgs} = parse_args(Args),
    case test:run(augment_opts(Opts)) of
        ok ->
            io:format("Tests passed!~n"),
            erlang:halt(0);
        Error ->
            io:format("Tests failed with error ~p~n", [Error]),
            erlang:halt(1)
    end.

%% @private
parse_args(Args) ->
    parse_args(Args, ?DEFAULT_OPTS, []).

%% @private
parse_args([], Accum, PArgs) ->
    {Accum, lists:reverse(PArgs)};
parse_args(["-r", Root | T], Accum, Pargs) ->
    parse_args(T, Accum#{root => Root}, Pargs);
parse_args(["-v" | T], Accum, Pargs) ->
    parse_args(T, Accum#{verbose => true}, Pargs);
parse_args(["-d" | T], Accum, Pargs) ->
    parse_args(T, Accum#{debug => true}, Pargs);
parse_args([H | T], Accum, Pargs) ->
    parse_args(T, Accum, [H | Pargs]).

%% @private
augment_opts(Opts) ->
    Root = maps:get(root, Opts),
    Opts#{
        apps_dir => test:make_path([Root, "apps"])
    }.
