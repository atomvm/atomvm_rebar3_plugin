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
-module(atomvm_packbeam_provider).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-include_lib("kernel/include/file.hrl").

-define(PROVIDER, packbeam).
-define(DEPS, [bootstrap]).
-define(OPTS, [
    {external, $e, "external", string, "External AVM modules"},
    {force, $f, "force", boolean, "Force rebuild"},
    {prune, $p, "prune", boolean, "Prune unreferenced BEAM files"},
    {start, $s, "start", atom, "Start module"},
    {remove_lines, $r, "remove_lines", boolean, "Remove line information from generated AVM files (off by default)"},
    {list, $l, "list", boolean, "List the contents of AVM files after creation"}
]).

-define(DEFAULT_OPTS, #{
    external_avms => [],
    force => false,
    prune => false,
    start => undefined,
    remove_lines => false,
    list => false
}).

-record(file_set, {
    name, out_dir, beam_files, priv_files, app_file
}).

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
        {example, "rebar3 atomvm packbeam"},
        % list of options understood by the plugin
        {opts, ?OPTS},
        {short_desc, "Create an AtomVM packbeam file"},
        {desc,
            "~n"
            "Use this plugin to create an AtomVM packbeam file from your rebar3 project.~n"
        }
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    try
        Opts = get_opts(State),
        rebar_api:debug("Effective opts for ~p: ~p", [?PROVIDER, Opts]),
        ok = do_packbeam(
            rebar_state:project_apps(State),
            lists:usort(rebar_state:all_deps(State)),
            maps:get(external_avms, Opts),
            maps:get(prune, Opts),
            maps:get(force, Opts),
            get_start_module(Opts),
            not maps:get(remove_lines, Opts),
            maps:get(list, Opts)
        ),
        {ok, State}
    catch
        C:E:S ->
            rebar_api:error("An error occurred in the ~p task.  Class=~p Error=~p Stacktrace=~p~n", [?PROVIDER, C, E, S]),
            {error, E}
    end.

get_start_module(Opts) ->
    case maps:get(start, Opts, undefined) of
        undefined -> undefined;
        StartModule -> StartModule
    end.

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
    SquashedOpts = atomvm_rebar3_plugin:proplist_to_map(squash_external_avms(ParsedArgs)),
    maps:merge(?DEFAULT_OPTS, maps:merge(RebarOpts, SquashedOpts)).

%% @private
squash_external_avm({external, AVMPath}, Accum) ->
    case filelib:is_file(AVMPath) of
        true ->
            case proplists:get_value(external_avms, Accum) of
                undefined ->
                    [{external_avms, [AVMPath]} | Accum];
                OtherAVMs ->
                    [{external_avms, [AVMPath | OtherAVMs]} | proplists:delete(external_avms, Accum)]
            end;
        _ ->
            throw({enoent, AVMPath})
    end;
squash_external_avm(Any, Accum) ->
    [Any | Accum].

%% @private
squash_external_avms(ParsedArgs) ->
    lists:foldl(
        fun squash_external_avm/2,
        [],
        ParsedArgs
    ).

%% @private
do_packbeam(ProjectApps, Deps, ExternalAVMs, Prune, Force, StartModule, IncludeLines, List) ->
    DepFileSets = [get_files(Dep) || Dep <- Deps],
    ProjectAppFileSets = [get_files(ProjectApp) || ProjectApp <- ProjectApps],
    DepsAvms = [
        maybe_create_packbeam(DepFileSet, [], false, Force, undefined, IncludeLines, false)
        || DepFileSet <- DepFileSets
    ],
    [
        maybe_create_packbeam(
            ProjectAppFileSet, DepsAvms ++ ExternalAVMs, Prune, Force, StartModule, IncludeLines, List
        )
        || ProjectAppFileSet <- ProjectAppFileSets
    ],
    ok.

%% @private
get_files(App) ->
    EBinDir = rebar_app_info:ebin_dir(App),
    BeamFiles = get_beam_files(EBinDir),
    AppFile = get_app_file(EBinDir),
    OutDir = rebar_app_info:out_dir(App),
    PrivFiles = get_all_files(filename:join(OutDir, "priv")),
    Name = binary_to_list(rebar_app_info:name(App)),
    #file_set{
        name = Name,
        out_dir = OutDir,
        beam_files = BeamFiles,
        app_file = AppFile,
        priv_files = PrivFiles
    }.

%% @private
get_beam_files(Dir) ->
    [filename:join(Dir, Mod) || Mod <- filelib:wildcard("*.beam", Dir)].

%% @private
get_app_file(Dir) ->
    AppFiles = [filename:join(Dir, Mod) || Mod <- filelib:wildcard("*.app", Dir)],
    case AppFiles of
        [] ->
            rebar_api:warn("No .app files in ~s", [Dir]),
            undefined;
        [AppFile] ->
            AppFile;
        [AppFile|_] ->
            rebar_api:warn("Multiple .app files in ~s (using first): ~p", [Dir, AppFiles]),
            AppFile
    end.

%% @private
get_all_files(Dir) ->
    AllFiles = [filename:join(Dir, Mod) || Mod <- filelib:wildcard("*", Dir)],
    RegularFiles = lists:filter(
        fun(Path) ->
            filelib:is_regular(Path)
        end,
        AllFiles
    ),
    SubDirs = lists:filter(
        fun(Path) ->
            filelib:is_dir(Path)
        end,
        AllFiles
    ),
    SubFiles = lists:foldl(
        fun(SubDir, Accum) ->
            get_all_files(SubDir) ++ Accum
        end,
        [],
        SubDirs
    ),
    RegularFiles ++ SubFiles.

%% @private
maybe_create_packbeam(FileSet, AvmFiles, Prune, Force, StartModule, IncludeLines, List) ->
    #file_set{
        name = Name,
        out_dir = OutDir,
        beam_files = BeamFiles,
        app_file = AppFile,
        priv_files = PrivFiles
    } = FileSet,
    DirName = filename:dirname(OutDir),
    TargetAVM = filename:join(DirName, Name ++ ".avm"),
    AppFiles = case AppFile of undefined -> []; _ -> [AppFile] end,
    case Force orelse needs_build(TargetAVM, BeamFiles ++ PrivFiles ++ AvmFiles ++ AppFiles) of
        true ->
            create_packbeam(FileSet, AvmFiles, Prune, StartModule, IncludeLines, List);
        _ ->
            rebar_api:debug("No packbeam build needed.", []),
            TargetAVM
    end.

%% @private
needs_build(Path, PathList) ->
    not filelib:is_file(Path) orelse
        modified_time(Path) < latest_modified_time(PathList).

%% @private
modified_time(Path) ->
    {ok, #file_info{mtime = MTime}} = file:read_file_info(Path, [{time, posix}]),
    MTime.

%% @private
latest_modified_time(PathList) ->
    lists:max([modified_time(Path) || Path <- PathList]).

%% @private
create_packbeam(FileSet, AvmFiles, Prune, StartModule, IncludeLines, List) ->
    #file_set{
        name = Name,
        out_dir = OutDir,
        beam_files = BeamFiles,
        app_file = AppFile,
        priv_files = PrivFiles
    } = FileSet,
    N = length(OutDir) + 1,
    PrivFilesRelative = [filename:join(Name, string:slice(PrivFile, N)) || PrivFile <- PrivFiles],
    {ApplicationModule, AppFileBinFiles} = create_app_file_bin_files(Name, OutDir, AppFile),
    BootFile = case StartModule of
        init ->
            [create_boot_file(OutDir, ApplicationModule)];
        _ ->
            []
    end,
    Cwd = rebar_dir:get_cwd(),
    try
        DirName = filename:dirname(OutDir),
        ok = file:set_cwd(DirName),
        AvmFilename = Name ++ ".avm",
        FileList = reorder_beamfiles(BeamFiles) ++ AppFileBinFiles ++ BootFile ++ PrivFilesRelative ++ AvmFiles,
        Opts = #{
            prune => Prune,
            start_module => StartModule,
            application_module => ApplicationModule,
            include_lines => IncludeLines
        },
        rebar_api:debug(
            "Creating AVM file ~p from FileList ~p.  Opts: ~p",
            [AvmFilename, FileSet, Opts]
        ),
        packbeam_api:create(
            AvmFilename, FileList, Opts
        ),
        rebar_api:info("AVM file written to ~s/~s", [DirName, AvmFilename]),
        maybe_list(DirName ++ "/" ++ AvmFilename, List),
        filename:join(DirName, AvmFilename)
    after
        ok = file:set_cwd(Cwd)
    end.

%% @private
maybe_list(_, false) ->
    ok;
maybe_list(AvmPath, _) ->
    AVMElements = packbeam_api:list(AvmPath),
    rebar_api:console("AVM contents~n============", []),
    lists:foreach(
        fun list_element/1,
        AVMElements
    ).

%% @private
list_element(AVMElement) ->
    ElementName = packbeam_api:get_element_name(AVMElement),
    ElementData = packbeam_api:get_element_data(AVMElement),
    rebar_api:console(
        "~s~s [~p]", [
            ElementName,
            case packbeam_api:is_entrypoint(AVMElement) of
                true -> " *";
                _ -> ""
            end,
            byte_size(ElementData)
        ]
    ).

%% @private
create_app_file_bin_files(_AppName, _OutDir, undefined) ->
    {undefined, []};
create_app_file_bin_files(AppName, OutDir, AppFile) ->
    case file:consult(AppFile) of
        {ok, Term} ->
            case Term of
                [{application, ApplicationModule, _Properties} = ApplicationSpec] ->
                    WritePath = filename:join([OutDir, "application.bin"]),
                    Bin = erlang:term_to_binary(ApplicationSpec),
                    ok = file:write_file(WritePath, Bin),
                    {ApplicationModule, [{WritePath, filename:join([AppName, "priv", "application.bin"])}]};
                _ ->
                    rebar_api:warn("Consulted app file (~s) does not appear to be an app spec.", [AppFile]),
                    {undefined, []}
            end;
        Error ->
            rebar_api:warn("Failed to consult app file : ~s Error: ~p", [AppFile, Error]),
            {undefined, []}
    end.

%% @private
create_boot_file(OutDir, ApplicationModule) ->
    Version = {0, 1, 0}, %% TODO
    BootSpec = {boot, Version, #{applications => [ApplicationModule]}},
    WritePath = filename:join([OutDir, "start.boot"]),
    Bin = erlang:term_to_binary(BootSpec),
    ok = file:write_file(WritePath, Bin),
    {WritePath, filename:join(["init", "priv", "start.boot"])}.

%% @private
reorder_beamfiles(BeamFiles) ->
    ExportPaths = [{exports(Path), Path} || Path <- BeamFiles],
    SortedExportPaths = lists:sort(fun compare_beams/2, ExportPaths),
    [Path || {_Exports, Path} <- SortedExportPaths].

%% @private
exports(Path) ->
    {ok, {_Module, ExportChunk}} = beam_lib:chunks(Path, [exports]),
    proplists:get_value(exports, ExportChunk).

%% @private
compare_beams({Exports1, _}, _) ->
    lists:member({start, 0}, Exports1).
