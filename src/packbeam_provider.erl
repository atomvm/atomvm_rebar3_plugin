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
-module(packbeam_provider).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-include_lib("kernel/include/file.hrl").

-define(PROVIDER, packbeam).
-define(DEPS, [compile]).
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
        {short_desc, "A rebar plugin to create packbeam files"},
        {desc, "A rebar plugin to create packbeam files"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case parse_args(rebar_state:command_args(State)) of
        {ok, Opts} ->
            do_packbeam(
                rebar_state:project_apps(State),
                lists:usort(rebar_state:all_deps(State)),
                maps:get(external_avms, Opts),
                maps:get(prune, Opts),
                maps:get(force, Opts),
                get_start_module(Opts),
                maps:get(include_lines, Opts)
            ),
            {ok, State};
        {error, Reason} ->
            io:format("~p~n", [Reason]),
            {error, Reason}
    end.

get_start_module(Opts) ->
    case maps:get(start_module, Opts, undefined) of
        undefined -> undefined;
        StartModule -> list_to_atom(StartModule)
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%
%% internal functions
%%

%% @private
parse_args(Args) ->
    Defaults = #{
        external_avms => [],
        prune => false,
        force => false,
        start_module => undefined,
        include_lines => true
    },
    parse_args(Args, Defaults).

%% @private
parse_args([], Accum) ->
    {ok, Accum};
parse_args(["-e", AVMPath | Rest], Accum) ->
    case filelib:is_file(AVMPath) of
        true ->
            parse_args(Rest, Accum#{external_avms => [AVMPath | maps:get(external_avms, Accum)]});
        _ ->
            {error, io_lib:format("File does not exist: ~p", [AVMPath])}
    end;
parse_args(["--external", AVMPath | Rest], Accum) ->
    case filelib:is_file(AVMPath) of
        true ->
            parse_args(Rest, Accum#{external_avms => [AVMPath | maps:get(external_avms, Accum)]});
        _ ->
            {error, io_lib:format("File does not exist: ~s", [AVMPath])}
    end;
parse_args(["-p" | Rest], Accum) ->
    parse_args(Rest, Accum#{prune => true});
parse_args(["--prune" | Rest], Accum) ->
    parse_args(Rest, Accum#{prune => true});
parse_args(["-f" | Rest], Accum) ->
    parse_args(Rest, Accum#{force => true});
parse_args(["--force" | Rest], Accum) ->
    parse_args(Rest, Accum#{force => true});
parse_args(["-s", StartModule | Rest], Accum) ->
    parse_args(Rest, Accum#{start_module => StartModule});
parse_args(["--start", StartModule | Rest], Accum) ->
    parse_args(Rest, Accum#{start_module => StartModule});
parse_args(["-i" | Rest], Accum) ->
    parse_args(["--include_lines" | Rest], Accum);
parse_args(["--include_lines" | Rest], Accum) ->
    io:format("Note.  The -i flag is deprecated.  Lines are now included by default.  Use -r|--remove_lines to remove lines from generated files.~n"),
    parse_args(Rest, Accum#{include_lines => true});
parse_args(["-r" | Rest], Accum) ->
    parse_args(Rest, Accum#{include_lines => false});
parse_args(["--remove_lines" | Rest], Accum) ->
    parse_args(Rest, Accum#{include_lines => false});
parse_args([_ | Rest], Accum) ->
    parse_args(Rest, Accum).

-record(file_set, {
    name, out_dir, beam_files, priv_files, app_file
}).

%% @private
do_packbeam(ProjectApps, Deps, ExternalAVMs, Prune, Force, StartModule, IncludeLines) ->
    DepFileSets = [get_files(Dep) || Dep <- Deps],
    ProjectAppFileSets = [get_files(ProjectApp) || ProjectApp <- ProjectApps],
    try
        DepsAvms = [
            maybe_create_packbeam(DepFileSet, [], false, Force, undefined, IncludeLines)
         || DepFileSet <- DepFileSets
        ],
        [
            maybe_create_packbeam(
                ProjectAppFileSet, DepsAvms ++ ExternalAVMs, Prune, Force, StartModule, IncludeLines
            )
         || ProjectAppFileSet <- ProjectAppFileSets
        ]
    catch
        _:E:S ->
            rebar_api:error("Packbeam creation failed: ~p : ~p", [E, S])
    end.

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
maybe_create_packbeam(FileSet, AvmFiles, Prune, Force, StartModule, IncludeLines) ->
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
            create_packbeam(FileSet, AvmFiles, Prune, StartModule, IncludeLines);
        _ ->
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
create_packbeam(FileSet, AvmFiles, Prune, StartModule, IncludeLines) ->
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
        packbeam_api:create(
            AvmFilename,
            FileList,
            #{
                prune => Prune,
                start_module => StartModule,
                application_module => ApplicationModule,
                include_lines => IncludeLines
            }
        ),
        rebar_api:info("AVM file written to : ~s", [AvmFilename]),
        filename:join(DirName, AvmFilename)
    after
        ok = file:set_cwd(Cwd)
    end.

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
