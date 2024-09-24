%%
%% Copyright (c) 2020-2023 Fred Dushin <fred@dushin.net>
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
-module(atomvm_version_provider).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-include_lib("kernel/include/file.hrl").

-define(PROVIDER, version).
-define(DEPS, []).
-define(OPTS, []).

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
        {example, "rebar3 atomvm version"},
        % list of options understood by the plugin
        {opts, ?OPTS},
        {short_desc, "Print the version of this plugin to the console."},
        {desc,
            "~n"
            "Use this task to print the version of this plugin to the console.~n"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    try
        case lists:keyfind(atomvm_rebar3_plugin, 1, application:loaded_applications()) of
            {_, _, Version} ->
                rebar_api:console(Version, []);
            false ->
                rebar_api:error(
                    "Error!  Unable to find atomvm_rebar3_plugin in loaded applications", []
                )
        end,
        {ok, State}
    catch
        C:E:S ->
            rebar_api:error(
                "An error occurred in the ~p task.  Class=~p Error=~p Stacktrace=~p~n", [
                    ?PROVIDER, C, E, S
                ]
            ),
            {error, E}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
