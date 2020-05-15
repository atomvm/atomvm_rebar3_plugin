%%
%% Copyright (c) dushin.net
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     * Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     * Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     * Neither the name of dushin.net nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY dushin.net ``AS IS'' AND ANY
%% EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%% DISCLAIMED. IN NO EVENT SHALL dushin.net BE LIABLE FOR ANY
%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%
-module(packbeam_provider).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-include_lib("kernel/include/file.hrl").

-define(PROVIDER, packbeam).
-define(DEPS, [compile]).
-define(OPTS, [
    {ext, $e, "external", undefined, "External AVM modules"},
    {force, $f, "force", undefined, "Force rebuild"}
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
    {Args, _} = rebar_state:command_parsed_args(State),
    Force = proplists:get_value(force, Args, false),
    case get_external_avms(rebar_state:command_args(State)) of
        {ok, ExternalAVMs} ->
            do_packbeam(
                rebar_state:project_apps(State),
                lists:usort(rebar_state:all_deps(State)),
                ExternalAVMs,
                Force
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
get_external_avms(Args) ->
    get_external_avms(Args, []).

%% @private
get_external_avms([], Accum) ->
    {ok, lists:reverse(Accum)};
get_external_avms(["-e", AVMPath|Rest], Accum) ->
    case filelib:is_file(AVMPath) of
        true ->
            get_external_avms(Rest, [AVMPath|Accum]);
        _ ->
            {error, io_lib:format("File does not exist: ~p", [AVMPath])}
    end;
get_external_avms(["--external", AVMPath|Rest], Accum) ->
    case filelib:is_file(AVMPath) of
        true ->
            get_external_avms(Rest, [AVMPath|Accum]);
        _ ->
            {error, io_lib:format("File does not exist: ~s", [AVMPath])}
    end;
get_external_avms([_|Rest], Accum) ->
    get_external_avms(Rest, Accum).

-record(file_set, {
    name, out_dir, beam_files, priv_files
}).

%% @private
do_packbeam(ProjectApps, Deps, ExternalAVMs, Force) ->
    DepFileSets = [get_files(Dep) || Dep <- Deps],
    ProjectAppFileSets = [get_files(ProjectApp) || ProjectApp <- ProjectApps],
    try
        DepsAvms = [maybe_create_packbeam(DepFileSet, [], Force) || DepFileSet <- DepFileSets],
        [maybe_create_packbeam(ProjectAppFileSet, DepsAvms ++ ExternalAVMs, Force) ||
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
maybe_create_packbeam(FileSet, AvmFiles, Force) ->
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
            create_packbeam(FileSet, AvmFiles);
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
create_packbeam(FileSet, AvmFiles) ->
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
        packbeam:create(AvmFilename, reorder_beamfiles(BeamFiles) ++ PrivFilesRelative ++ AvmFiles),
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
