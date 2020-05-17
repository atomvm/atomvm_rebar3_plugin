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
    {prune, $p, "prune", undefined, "Prune unreferenced BEAM files"}
]).

%%
%% provider implementation
%%
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 packbeam"}, % How to use the plugin
            {opts, ?OPTS},                   % list of options understood by the plugin
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
                maps:get(force, Opts)
            ),
            {ok, State};
        {error, Reason} ->
            io:format("~p~n", [Reason]),
            {error, Reason}
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%
%% internal functions
%%

%% @private
parse_args(Args) ->
    parse_args(Args, #{external_avms => [], prune => false, force => false}).

%% @private
parse_args([], Accum) ->
    {ok, Accum};
parse_args(["-e", AVMPath|Rest], Accum) ->
    case filelib:is_file(AVMPath) of
        true ->
            parse_args(Rest, Accum#{external_avms => [AVMPath | maps:get(external_avms, Accum)]});
        _ ->
            {error, io_lib:format("File does not exist: ~p", [AVMPath])}
    end;
parse_args(["--external", AVMPath|Rest], Accum) ->
    case filelib:is_file(AVMPath) of
        true ->
            parse_args(Rest, Accum#{external_avms => [AVMPath | maps:get(external_avms, Accum)]});
        _ ->
            {error, io_lib:format("File does not exist: ~s", [AVMPath])}
    end;
parse_args(["-p"|Rest], Accum) ->
    parse_args(Rest, Accum#{prune => true});
parse_args(["--prune"|Rest], Accum) ->
    parse_args(Rest, Accum#{prune => true});
parse_args(["-f"|Rest], Accum) ->
    parse_args(Rest, Accum#{force => true});
parse_args(["--force"|Rest], Accum) ->
    parse_args(Rest, Accum#{force => true});
parse_args([_|Rest], Accum) ->
    parse_args(Rest, Accum).

-record(file_set, {
    name, out_dir, beam_files, priv_files
}).

%% @private
do_packbeam(ProjectApps, Deps, ExternalAVMs, Prune, Force) ->
    DepFileSets = [get_files(Dep) || Dep <- Deps],
    ProjectAppFileSets = [get_files(ProjectApp) || ProjectApp <- ProjectApps],
    try
        DepsAvms = [maybe_create_packbeam(DepFileSet, [], false, Force) || DepFileSet <- DepFileSets],
        [maybe_create_packbeam(ProjectAppFileSet, DepsAvms ++ ExternalAVMs, Prune, Force) ||
            ProjectAppFileSet <- ProjectAppFileSets]
    catch
        _:E ->
             rebar_api:error("Packbeam creation failed: ~p", [E])
    end.

%% @private
get_files(App) ->
    EBinDir = rebar_app_info:ebin_dir(App),
    BeamFiles = get_beam_files(EBinDir),
    OutDir = rebar_app_info:out_dir(App),
    PrivFiles = get_all_files(filename:join(OutDir, "priv")),
    Name = binary_to_list(rebar_app_info:name(App)),
    #file_set{
        name=Name,
        out_dir=OutDir,
        beam_files=BeamFiles,
        priv_files=PrivFiles
    }.

%% @private
get_beam_files(Dir) ->
    [filename:join(Dir, Mod) || Mod <- filelib:wildcard("*.beam", Dir)].

%% @private
get_all_files(PrivDir) ->
    AllPaths = [filename:join(PrivDir, Mod) || Mod <- filelib:wildcard("*", PrivDir)],
    lists:filter(
        fun(Path) ->
            filelib:is_file(Path)
        end,
        AllPaths
    ).

%% @private
maybe_create_packbeam(FileSet, AvmFiles, Prune, Force) ->
    #file_set{
        name=Name,
        out_dir=OutDir,
        beam_files=BeamFiles,
        priv_files=PrivFiles
    } = FileSet,
    DirName = filename:dirname(OutDir),
    TargetAVM = filename:join(DirName, Name ++ ".avm"),
    case Force orelse needs_build(TargetAVM, BeamFiles ++ PrivFiles ++ AvmFiles) of
        true ->
            create_packbeam(FileSet, AvmFiles, Prune);
        _ ->
            TargetAVM
    end.

%% @private
needs_build(Path, PathList)  ->
    not filelib:is_file(Path) orelse
        modified_time(Path) < latest_modified_time(PathList).

%% @private
modified_time(Path) ->
    {ok, #file_info{mtime=MTime}} = file:read_file_info(Path, [{time, posix}]),
    MTime.

%% @private
latest_modified_time(PathList) ->
    lists:max([modified_time(Path) || Path <- PathList]).

%% @private
create_packbeam(FileSet, AvmFiles, Prune) ->
    #file_set{
        name=Name,
        out_dir=OutDir,
        beam_files=BeamFiles,
        priv_files=PrivFiles
    } = FileSet,
    N = length(OutDir) + 1,
    PrivFilesRelative = [filename:join(Name, string:slice(PrivFile, N)) || PrivFile <- PrivFiles],
    Cwd = rebar_dir:get_cwd(),
    try
        DirName = filename:dirname(OutDir),
        ok = file:set_cwd(DirName),
        AvmFilename = Name ++ ".avm",
        packbeam:create(AvmFilename, reorder_beamfiles(BeamFiles) ++ PrivFilesRelative ++ AvmFiles, Prune),
        rebar_api:info("AVM file written to : ~s", [AvmFilename]),
        filename:join(DirName, AvmFilename)
    after
        ok = file:set_cwd(Cwd)
    end.

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
