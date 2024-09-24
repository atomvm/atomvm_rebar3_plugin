%%
%% Copyright (c) 2023 Fred Dushin <fred@dushin.net>
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
-module(atomvm_bootstrap_provider).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, bootstrap).
-define(DEPS, [{default, compile}]).
-define(OPTS, [
    {bootstrap_dir, $b, "bootstrap_dir", boolean, "Bootstrap directory"},
    {force, $f, "force", boolean, "Force rebuild"}
]).

-define(DEFAULT_OPTS, #{
    bootstrap_dir => undefined,
    force => false
}).

-include_lib("kernel/include/file.hrl").

%%
%% provider implementation
%%

%% @hidden
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
        {example, "rebar3 atomvm bootstrap"},
        % list of options understood by the plugin
        {opts, ?OPTS},
        {short_desc, "Compiles bootstrap .erl files"},
        {desc,
            "~n"
            "This plugin is used internally by the atomvm packbeam task to compile~n"
            "modules that cannot be compiled directly by rebar.~n"
            "~n"
            "Users typically have no reason to use this task directly.~n"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

%% @hidden
-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    try
        Opts = get_opts(State),
        rebar_api:debug("Effective opts for ~p: ~p", [?PROVIDER, Opts]),
        ok = do_bootstrap(
            rebar_state:project_apps(State),
            maps:get(bootstrap_dir, Opts),
            maps:get(force, Opts)
        ),
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

%% @hidden
-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%
%% internal functions
%%

%% @private
get_opts(State) ->
    {ParsedArgs, _} = rebar_state:command_parsed_args(State),
    RebarOpts = atomvm_rebar3_plugin:get_atomvm_rebar_provider_config(State, ?PROVIDER),
    ParsedOpts = atomvm_rebar3_plugin:proplist_to_map(ParsedArgs),
    maps:merge(
        ?DEFAULT_OPTS,
        maps:merge(RebarOpts, ParsedOpts)
    ).

%% @private
do_bootstrap(ProjectApps, BootstrapDir, Force) ->
    lists:foreach(
        fun(ProjectApp) ->
            do_bootstrap_app(ProjectApp, BootstrapDir, Force)
        end,
        ProjectApps
    ).

%% @private
do_bootstrap_app(App, undefined, Force) ->
    Dir = rebar_app_info:dir(App),
    do_bootstrap_app(App, filename:join(Dir, "bootstrap"), Force);
do_bootstrap_app(App, BootstrapDir, Force) ->
    % Dir = rebar_app_info:dir(App),
    case filelib:is_dir(BootstrapDir) of
        true ->
            BootstrapFiles = [
                filename:join(BootstrapDir, Mod)
             || Mod <- filelib:wildcard("*.erl", BootstrapDir)
            ],
            lists:foreach(
                fun(BootstrapFile) ->
                    do_bootstrap_file(App, BootstrapFile, Force)
                end,
                BootstrapFiles
            );
        _ ->
            rebar_api:debug("No bootstrap dir ~s.  Skipping...", [BootstrapDir]),
            ok
    end.

%% @private
do_bootstrap_file(App, BootstrapFile, Force) ->
    OutDir = rebar_app_info:out_dir(App),
    EBinDir = filename:join(OutDir, "bootstrap_ebin"),
    ok = ensure_path(EBinDir),
    maybe_compile(App, BootstrapFile, EBinDir, Force).

%% @private
ensure_path(Dir) ->
    case filelib:is_dir(Dir) of
        true ->
            ok;
        _ ->
            ok = filelib:ensure_dir(Dir),
            ok = file:make_dir(Dir)
    end.

%% @private
maybe_compile(App, BootstrapFile, EBinDir, Force) ->
    Basename = filename:basename(BootstrapFile, ".erl"),
    BeamFile = filename:join(EBinDir, Basename ++ ".beam"),
    case Force orelse needs_build(BootstrapFile, BeamFile) of
        true ->
            CompilerOptions =
                [{outdir, EBinDir}, report] ++
                    get_compiler_options(App),
            rebar_api:debug("Compiling bootstrap file ~s with options ~p ...", [
                BootstrapFile, CompilerOptions
            ]),
            case
                compile:file(
                    BootstrapFile, CompilerOptions
                )
            of
                {ok, ModuleName} ->
                    rebar_api:debug("Compiled bootstrap module ~p", [ModuleName]),
                    ok;
                Error ->
                    rebar_api:error("Failed to compile bootstrap file ~s: ~p", [
                        BootstrapFile, Error
                    ]),
                    Error
            end;
        _ ->
            rebar_api:debug("bootstrap file ~s not in need of rebuild", [BootstrapFile]),
            ok
    end.

%% @private
get_compiler_options(App) ->
    Opts = rebar_app_info:opts(App),
    case dict:find(erl_opts, Opts) of
        {ok, ErlOpts} ->
            ErlOpts;
        _ ->
            []
    end.

%% @private
needs_build(Path, BeamFile) ->
    not filelib:is_file(BeamFile) orelse
        modified_time(Path) > modified_time(BeamFile).

%% @private
modified_time(Path) ->
    {ok, #file_info{mtime = MTime}} = file:read_file_info(Path, [{time, posix}]),
    MTime.
