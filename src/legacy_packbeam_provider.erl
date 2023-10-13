%%
%% Copyright (c) dushin.net
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
-module(legacy_packbeam_provider).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, packbeam).
-define(DEPS, [{default, compile}]).
-define(OPTS, [
    {ext, $e, "external", undefined, "External AVM modules"},
    {force, $f, "force", undefined, "Force rebuild"},
    {prune, $p, "prune", undefined, "Prune unreferenced BEAM files"},
    {include_lines, $i, "include_lines", undefined, "Include line information in generated AVM files (deprecated)"},
    {remove_lines, $r, "remove_lines", undefined, "Remove line information from generated AVM files (off by default)"},
    {start, $s, "start", atom, "Start module"}
]).

%%
%% provider implementation
%%
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        % The 'user friendly' name of the task
        {name, ?PROVIDER},
        % The module implementation of the task
        {module, ?MODULE},
        % The task can be run by the user, always true
        {bare, true},
        % The list of dependencies
        {deps, ?DEPS},
        % How to use the plugin
        {example, "rebar3 packbeam"},
        % list of options understood by the plugin
        {opts, ?OPTS},
        {short_desc, "A rebar plugin to create packbeam files (DEPRECATED)"},
        {desc, "A rebar plugin to create packbeam files (DEPRECATED)"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:warn("DEPRECATED The packbeam tool has been moved under the atomvm namespace", []),
    atomvm_packbeam_provider:do(State).

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    atomvm_packbeam_provider:format_error(Reason).

