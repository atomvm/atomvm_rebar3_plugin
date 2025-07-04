%% Copyright (c) 2025 Uncle Grumpy <winford@object.stream>
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
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%%
-module(atomvm_dialyzer_provider).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, dialyzer).
-define(DEPS, [{default, compile}, {default, app_discovery}]).
-define(OPTS, [
    %% TODO: implement incremental PLTs
    {base_plt_location, undefined, "base-plt-location", string,
        "The location of base PLT file, defaults to $HOME/.cache/rebar3"},
    {plt_location, undefined, "plt-location", string,
        "The location of the PLT file, defaults to the profile's base directory"},
    {plt_prefix, undefined, "plt-prefix", string,
        "The prefix to the PLT file, defaults to application name"},
    {atomvm_root, undefined, "atomvm_root", string,
        "Base path to AtomVM install directory (i.e. /opt/atomvm), only needed if the \"atomvm\" launcher script is not in PATH."}
    %% TODO: implement statistics
    %{statistics, undefined, "statistics", boolean, "Print information about the progress of execution. Default: false" }]
]).
-define(LIBS, [alisp, eavmlib, estdlib, etest]).

%% ===================================================================
%% Public API
%% ===================================================================
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
        {example, "rebar3 atomvm dialyzer"},
        % list of options understood by the plugin
        {opts, ?OPTS},
        {short_desc, "Dialyze an AtomVM application"},
        {desc,
            "~n"
            "Use this plugin to check an AtomVM application for type discrepancies.~n"
            "This plugin depends on an installed atomvm launcher in the users PATH.~n"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    try
        Config = get_opts(State),
        rebar_api:debug("Effective opts for ~p: ~p", [?PROVIDER, Config]),
        ok = do_dialize(Config, State),
        {ok, State}
    catch
        C:E:S ->
            rebar_api:error("An error (~p) occurred in the ~p task.", [E, ?PROVIDER]),
            rebar_api:debug("Class=~p Error=~p~nSTACKTRACE:~n~p~n", [C, E, S]),
            {error, E}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Internal functions
%% ===================================================================

%% @private
-spec get_opts(State :: rebar_state:t()) -> map().
get_opts(State) ->
    {ParsedArgs, _} = rebar_state:command_parsed_args(State),
    RebarOpts = atomvm_rebar3_plugin:get_atomvm_rebar_provider_config(State, ?PROVIDER),
    ParsedOpts = atomvm_rebar3_plugin:proplist_to_map(ParsedArgs),
    maps:merge(RebarOpts, ParsedOpts).

%% @private
-spec print_warnings(Warnings :: [any(), ...]) -> ok.
print_warnings(Warnings) ->
    lists:foreach(
        fun(Warning) ->
            io:format("~s", [dialyzer:format_warning(Warning)])
        end,
        Warnings
    ),
    ok.

%% @private
do_dialize(Config, State) ->
    %% check base_plt exists and is up to date
    BasePLT = base_plt_absname(Config),
    case dialyzer:plt_info(BasePLT) of
        {ok, _} ->
            check_base_plt(Config);
        _ ->
            do_build_base_plt(Config)
    end,

    %% dialyze sources
    PLT = plt_absolute_name(State),
    case dialyzer:plt_info(PLT) of
        {ok, _} ->
            check_app_plt(State);
        _ ->
            do_build_plt(State)
    end,
    AppBEAMs = get_app_beam_abspath(State),
    rebar_api:info("Analyzing application with dialyzer...", []),
    try
        dialyzer:run([
            {analysis_type, succ_typings}, {files_rec, [AppBEAMs]}, {plts, [PLT, BasePLT]}
        ])
    of
        [] ->
            rebar_api:info("No problems found by dialyzer.~n", []),
            ok;
        Problems ->
            print_warnings(Problems)
    catch
        throw:{dialyzer_error, Reason} ->
            rebar_api:abort("Dialyzer failed! reason: ~p.~n", [Reason])
    end,
    ok.

% @private
check_base_plt(Config) ->
    rebar_api:info("Checking AtomVM base PLT...", []),
    PLT = base_plt_absname(Config),
    try
        dialyzer:run([
            {analysis_type, plt_check}, {init_plt, PLT}, {output_plt, PLT}
        ])
    of
        [] ->
            ok;
        _ ->
            ok = do_build_base_plt(Config)
    catch
        throw:{dialyzer_error, _} ->
            ok = do_build_base_plt(Config)
    end,
    ok.

% @private
base_plt_absname(Config) ->
    Home =
        case os:getenv("HOME") of
            false ->
                rebar_api:abort("Unable to locate users home directory", []);
            Path ->
                string:trim(Path)
        end,
    Base = maps:get(base_plt_location, Config, filename:join(Home, ".cache/rebar3")),
    Version = atomvm_version(Config),
    BasePLT = filename:absname_join(filename:absname(Base), "AtomVM-" ++ Version ++ ".plt"),
    rebar_api:debug("Base PLT file: ~p", [BasePLT]),
    string:trim(BasePLT).

% @private
do_build_base_plt(Config) ->
    rebar_api:info("Building AtomVM base PLT...", []),
    %% build plt
    BEAMdir = get_base_beam_path(Config),
    PLT = base_plt_absname(Config),
    try
        dialyzer:run([
            {analysis_type, plt_build}, {init_plt, PLT}, {output_plt, PLT}, {files_rec, BEAMdir}
        ])
    of
        [] ->
            ok;
        Failure ->
            print_warnings(Failure),
            rebar_api:abort("Failed to create project plt!~n", [])
    catch
        throw:{dialyzer_error, Error} ->
            rebar_api:abort("Failed to crete plt, error:~p~n", [Error])
    end.

% @private
check_app_plt(State) ->
    rebar_api:info("Checking application PLT...", []),
    PLT = plt_absolute_name(State),
    try dialyzer:run([{analysis_type, plt_check}, {plts, [PLT]}]) of
        _ ->
            ok
    catch
        throw:{dialyzer_error, _} ->
            ok = do_build_plt(State)
    end,
    ok.

% @private
do_build_plt(State) ->
    rebar_api:info("Building application PLT...", []),
    %% build plt
    BEAMdir = string:trim(get_app_beam_abspath(State)),
    PLT = string:trim(plt_absolute_name(State)),
    try
        dialyzer:run([
            {analysis_type, plt_build},
            {output_plt, PLT},
            {files_rec, [BEAMdir]}
        ])
    of
        _ ->
            ok
    catch
        throw:{dialyzer_error, Error} ->
            rebar_api:abort("Failed to crete plt, error: ~p~n", [Error])
    end.

%% @private
plt_absolute_name(State) ->
    {App, Path} = app_profile_abs_dir(State),
    ProjectPLT = filename:absname_join(Path, App ++ ".plt"),
    rebar_api:debug("Project plt: ~p~n", [ProjectPLT]),
    string:trim(ProjectPLT).

%% @private
get_base_beam_path(Config) ->
    Path =
        case maps:get(atomvm_root, Config, []) of
            [] ->
                default_base_beam_path();
            Base ->
                get_base_beam_path_list(Base)
        end,
    rebar_api:debug("AtomVM beam PATH: ~p", [Path]),
    Path.

%% @private
get_app_beam_abspath(State) ->
    {App, WorkDir} = app_profile_abs_dir(State),
    BEAMdir = filename:absname_join(WorkDir, "lib/" ++ App ++ "/ebin"),
    rebar_api:debug("App beam path for plt: ~p", [BEAMdir]),
    BEAMdir.

%% @private
atomvm_install_path() ->
    BinPath =
        case os:find_executable("atomvm") of
            false ->
                rebar_api:abort("Path to AtomVM installation not found!", []);
            AtomVM ->
                AtomVM
        end,
    [Base, _] = string:split(BinPath, "bin"),
    filename:join(filename:absname(Base), "lib/atomvm").

%% @private
default_base_beam_path() ->
    AVMpath = atomvm_install_path(),
    get_base_beam_path_list(AVMpath).

%% @private
get_base_beam_path_list(Base) ->
    Libs =
        lists:foldl(
            fun(E, Acc) ->
                [
                    filelib:wildcard(
                        Base ++ "/{lib,libs}/" ++ atom_to_list(E) ++ "*/**/{ebin,beams}"
                    )
                    | Acc
                ]
            end,
            [],
            ?LIBS
        ),
    rebar_api:debug("AtomVM libraries to add to plt: ~p~n", [Libs]),
    NotFound = lists:duplicate(length(?LIBS), []),
    case Libs of
        NotFound ->
            rebar_api:abort("Unable to locate AtomVM beams", []);
        Ebins ->
            Ebins
    end.

atomvm_version(Config) ->
    Base = filename:absname(string:trim(maps:get(atomvm_root, Config, atomvm_install_path()))),
    Version =
        case filename:basename(Base) of
            "build" ->
                string:trim(os:cmd(filename:absname_join(Base, "src/AtomVM") ++ " -version"));
            _ ->
                string:trim(os:cmd("atomvm -version"))
        end,
    rebar_api:debug("AtomVM version = ~p", [Version]),
    Version.

% @private
app_profile_abs_dir(State) ->
    Profile =
        case rebar_state:current_profiles(State) of
            [Prof0 | _] ->
                Prof0;
            Prof1 when is_atom(Prof1) ->
                Prof1;
            Arg ->
                rebar_api:abort("Unable to determine rebar3 profile, got badarg ~p", [Arg])
        end,
    WorkDir = filename:absname(filename:join("_build", Profile)),
    ok = filelib:ensure_path(WorkDir),
    [App | _] =
        case rebar_state:current_app(State) of
            undefined ->
                rebar_state:project_apps(State);
            AppInfo ->
                [AppInfo]
        end,
    {binary_to_list(rebar_app_info:name(App)), WorkDir}.
