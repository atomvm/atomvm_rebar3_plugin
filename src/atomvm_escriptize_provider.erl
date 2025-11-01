%%
%% Copyright (c) 2025 Paul Guyot <pguyot@kallisys.net>
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

-module(atomvm_escriptize_provider).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-include_lib("kernel/include/file.hrl").

-define(PROVIDER, escriptize).
-define(DEPS, [packbeam]).
-define(OPTS, [
    {atomvm_binary, $b, "atomvm_binary", string, "Path to AtomVM binary (default: which AtomVM)"},
    {atomvmlib, $l, "atomvmlib", string, "Path to atomvmlib.avm"},
    {output, $o, "output", string, "Output executable name (default: app name)"},
    {objcopy, $c, "objcopy", string, "Path to objcopy tool (auto-detected if not specified)"},
    {start, $s, "start", atom, "Start module (default: app name)"}
]).

-define(DEFAULT_OPTS, #{
    atomvm_binary => undefined,
    atomvmlib => undefined,
    output => undefined,
    objcopy => undefined,
    start => undefined
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
        {example, "rebar3 atomvm escriptize"},
        % list of options understood by the plugin
        {opts, ?OPTS},
        {short_desc, "Create a standalone executable with embedded AVM"},
        {desc,
            "~n"
            "Use this plugin to create a standalone executable by embedding an AVM file "
            "into the AtomVM binary using objcopy.~n"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    try
        Opts = get_opts(State),
        rebar_api:debug("Effective opts for ~p: ~p", [?PROVIDER, Opts]),

        % Get app info
        [App] = [ProjectApp || ProjectApp <- rebar_state:project_apps(State)],
        OutDir = rebar_app_info:out_dir(App),
        Name = binary_to_list(rebar_app_info:name(App)),
        DirName = filename:dirname(OutDir),

        % Get paths
        TargetAVM = filename:join(DirName, Name ++ ".avm"),
        AtomVMLib = get_atomvmlib_path(Opts),
        AtomVMBinary = get_atomvm_binary(Opts),
        ObjCopyTool = get_objcopy_tool(Opts),
        OutputExe = get_output_path(Opts, DirName, Name),

        % Get start module (default to app name)
        StartModule =
            case maps:get(start, Opts) of
                undefined -> list_to_atom(Name);
                Module -> Module
            end,

        % Create packed AVM with atomvmlib
        PackedAVM = create_packed_avm(TargetAVM, AtomVMLib, DirName, Name, StartModule),

        % Copy AtomVM binary
        ok = copy_atomvm_binary(AtomVMBinary, OutputExe),

        % Embed AVM into executable
        ok = embed_avm(ObjCopyTool, OutputExe, PackedAVM),

        % Make executable
        ok = make_executable(OutputExe),

        rebar_api:info("Created standalone executable: ~s", [OutputExe]),
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

%%
%% internal functions
%%

%% @private
get_opts(State) ->
    {ParsedArgs, _} = rebar_state:command_parsed_args(State),
    RebarOpts = atomvm_rebar3_plugin:get_atomvm_rebar_provider_config(State, ?PROVIDER),
    ParsedOpts = atomvm_rebar3_plugin:proplist_to_map(ParsedArgs),
    maps:merge(?DEFAULT_OPTS, maps:merge(RebarOpts, ParsedOpts)).

%% @private
get_atomvmlib_path(Opts) ->
    case maps:get(atomvmlib, Opts) of
        undefined ->
            % Try to find atomvmlib.avm in common locations
            case find_atomvmlib() of
                {ok, Path} -> Path;
                {error, Reason} -> throw({atomvmlib_not_found, Reason})
            end;
        Path ->
            case filelib:is_file(Path) of
                true -> Path;
                false -> throw({atomvmlib_not_found, Path})
            end
    end.

%% @private
find_atomvmlib() ->
    % First try to infer from AtomVM wrapper script
    case os:find_executable("AtomVM") of
        false ->
            find_atomvmlib_fallback();
        WrapperPath ->
            case infer_atomvmlib_from_wrapper(WrapperPath) of
                {ok, Path} -> {ok, Path};
                {error, _} -> find_atomvmlib_fallback()
            end
    end.

%% @private
find_atomvmlib_fallback() ->
    % Try to find atomvmlib.avm in common locations
    PossiblePaths = [
        "/opt/local/lib/atomvm/atomvmlib.avm",
        "/usr/local/lib/atomvm/atomvmlib.avm",
        "/usr/lib/atomvm/atomvmlib.avm",
        filename:join([os:getenv("HOME", "/tmp"), ".atomvm", "lib", "atomvmlib.avm"])
    ],
    case lists:filter(fun filelib:is_file/1, PossiblePaths) of
        [Path | _] -> {ok, Path};
        [] -> {error, "Could not find atomvmlib.avm. Please specify with --atomvmlib option"}
    end.

%% @private
infer_atomvmlib_from_wrapper(WrapperPath) ->
    % The wrapper script references atomvmlib.avm at ${avm_lib}/atomvm/atomvmlib.avm
    % where avm_lib="${avm_root}/lib" and avm_root is the prefix
    Dir = filename:dirname(WrapperPath),
    Prefix = filename:dirname(Dir),
    AtomVMLibPath = filename:join([Prefix, "lib", "atomvm", "atomvmlib.avm"]),
    case filelib:is_file(AtomVMLibPath) of
        true -> {ok, AtomVMLibPath};
        false -> {error, not_found}
    end.

%% @private
get_atomvm_binary(Opts) ->
    case maps:get(atomvm_binary, Opts) of
        undefined ->
            % Use which to find AtomVM
            case find_atomvm_binary() of
                {ok, Path} -> Path;
                {error, Reason} -> throw({atomvm_binary_not_found, Reason})
            end;
        Path ->
            case filelib:is_file(Path) of
                true -> Path;
                false -> throw({atomvm_binary_not_found, Path})
            end
    end.

%% @private
find_atomvm_binary() ->
    case os:find_executable("AtomVM") of
        false ->
            {error, "AtomVM binary not found in PATH. Please specify with --atomvm_binary option"};
        Path ->
            % Check if it's a shell script wrapper and find the actual binary
            case resolve_atomvm_binary(Path) of
                {ok, BinaryPath} -> {ok, BinaryPath};
                % Fall back to original path
                {error, _} -> {ok, Path}
            end
    end.

%% @private
resolve_atomvm_binary(Path) ->
    % Try to read the file to see if it's a shell script
    case file:read_file(Path) of
        {ok, Content} ->
            case binary:match(Content, <<"#!/bin/sh">>) of
                {0, _} ->
                    % It's a shell script, parse it to find the actual binary
                    % The standard wrapper is at /prefix/bin/AtomVM
                    % The actual binary is at /prefix/lib/atomvm/AtomVM
                    Dir = filename:dirname(Path),
                    Prefix = filename:dirname(Dir),
                    ActualBinary = filename:join([Prefix, "lib", "atomvm", "AtomVM"]),
                    case filelib:is_file(ActualBinary) of
                        true -> {ok, ActualBinary};
                        false -> {error, not_found}
                    end;
                _ ->
                    {error, not_a_script}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private
get_objcopy_tool(Opts) ->
    case maps:get(objcopy, Opts) of
        undefined ->
            case find_objcopy() of
                {ok, Path} -> Path;
                {error, Reason} -> throw({objcopy_not_found, Reason})
            end;
        Path ->
            Path
    end.

%% @private
find_objcopy() ->
    % Try different objcopy variants
    Tools =
        case os:type() of
            {unix, darwin} ->
                % On macOS, prefer llvm-objcopy and try MacPorts variants
                [
                    "llvm-objcopy",
                    "llvm-objcopy-mp-21",
                    "llvm-objcopy-mp-20",
                    "llvm-objcopy-mp-19",
                    "objcopy"
                ];
            {unix, linux} ->
                % On Linux, prefer objcopy, then llvm-objcopy
                ["objcopy", "llvm-objcopy"];
            _ ->
                ["objcopy", "llvm-objcopy"]
        end,
    case find_first_executable(Tools) of
        {ok, Path} -> {ok, Path};
        error -> {error, "No objcopy tool found. Please install llvm or binutils"}
    end.

%% @private
find_first_executable([]) ->
    error;
find_first_executable([Tool | Rest]) ->
    case os:find_executable(Tool) of
        false -> find_first_executable(Rest);
        Path -> {ok, Path}
    end.

%% @private
get_output_path(Opts, DirName, Name) ->
    case maps:get(output, Opts) of
        undefined ->
            % Place executable in _build/default/bin/ like standard rebar3 escriptize
            BuildDir = filename:dirname(DirName),
            BinDir = filename:join(BuildDir, "bin"),
            ok = filelib:ensure_dir(filename:join(BinDir, "dummy")),
            filename:join(BinDir, Name);
        OutputName ->
            case filename:dirname(OutputName) of
                "." ->
                    BuildDir = filename:dirname(DirName),
                    BinDir = filename:join(BuildDir, "bin"),
                    ok = filelib:ensure_dir(filename:join(BinDir, "dummy")),
                    filename:join(BinDir, OutputName);
                _ ->
                    OutputName
            end
    end.

%% @private
create_packed_avm(TargetAVM, AtomVMLib, DirName, Name, StartModule) ->
    PackedAVM = filename:join(DirName, Name ++ "_packed.avm"),

    % Use packbeam_api to create a new AVM with atomvmlib and the app AVM
    rebar_api:debug("Creating packed AVM with atomvmlib: ~s (start: ~p)", [PackedAVM, StartModule]),

    % Read both AVM files
    case {filelib:is_file(AtomVMLib), filelib:is_file(TargetAVM)} of
        {true, true} ->
            packbeam_api:create(PackedAVM, [AtomVMLib, TargetAVM], #{start => StartModule}),
            rebar_api:info("Created packed AVM: ~s with start module ~p", [PackedAVM, StartModule]),
            PackedAVM;
        {false, _} ->
            throw({file_not_found, AtomVMLib});
        {_, false} ->
            throw({file_not_found, TargetAVM})
    end.

%% @private
copy_atomvm_binary(Source, Dest) ->
    rebar_api:debug("Copying AtomVM binary from ~s to ~s", [Source, Dest]),
    case file:copy(Source, Dest) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            throw({copy_failed, Source, Dest, Reason})
    end.

%% @private
embed_avm(ObjCopyTool, Executable, AVMFile) ->
    % Use section name without dot on Linux for automatic symbol generation
    % Use segment/section syntax on macOS
    SectionName =
        case os:type() of
            {unix, linux} -> "atomvm_avm";
            _ -> ".atomvm_avm"
        end,

    % Determine the objcopy command based on OS
    case os:type() of
        {unix, darwin} ->
            % On macOS, use segment/section syntax
            Cmd = lists:flatten(
                io_lib:format(
                    "~s --add-section __ATOMVM,__avm_data=~s ~s",
                    [ObjCopyTool, AVMFile, Executable]
                )
            ),
            rebar_api:debug("Embedding AVM with command: ~s", [Cmd]),
            run_objcopy_cmd(Cmd);
        {unix, linux} ->
            % On Linux: Step 1 - Add the section
            {ok, AVMInfo} = file:read_file_info(AVMFile),
            AVMSize = AVMInfo#file_info.size,

            Cmd1 = lists:flatten(
                io_lib:format(
                    "~s --add-section ~s=~s --set-section-flags ~s=alloc,load,readonly,data ~s",
                    [ObjCopyTool, SectionName, AVMFile, SectionName, Executable]
                )
            ),
            rebar_api:debug("Step 1 - Adding section: ~s", [Cmd1]),
            ok = run_objcopy_cmd(Cmd1),

            % Step 2 - Add symbols at section boundaries
            set_atomvm_avm_info(ObjCopyTool, Executable, SectionName, AVMSize);
        _ ->
            % Default to Linux syntax
            Cmd = lists:flatten(
                io_lib:format(
                    "~s --add-section ~s=~s --set-section-flags ~s=alloc,readonly,data ~s",
                    [ObjCopyTool, SectionName, AVMFile, SectionName, Executable]
                )
            ),
            rebar_api:debug("Embedding AVM with command: ~s", [Cmd]),
            run_objcopy_cmd(Cmd)
    end.

%% @private
run_objcopy_cmd(Cmd) ->
    case os:cmd(Cmd ++ " 2>&1") of
        "" ->
            ok;
        Output ->
            % Check if it's just a warning or an actual error
            case string:str(Output, "error") of
                0 ->
                    rebar_api:debug("objcopy output: ~s", [Output]),
                    ok;
                _ ->
                    throw({embed_failed, Output})
            end
    end.

%% @private
set_atomvm_avm_info(ObjCopyTool, Executable, SectionName, SectionSize) ->
    % Parse objdump to get the section's offset and length
    ObjdumpCmd = lists:flatten(
        io_lib:format("objdump -h ~s | grep '~s' | grep -v atomvm_avm_info", [
            Executable, SectionName
        ])
    ),
    rebar_api:debug("Step 2 - Get section offset and size: ~s", [ObjdumpCmd]),
    Output = os:cmd(ObjdumpCmd),
    % Parse output: " 18 atomvm_avm    000394ec  000000000014e8e5  ..."
    % Fields are: Idx Name Size VMA LMA Offset Alignment
    Fields = string:tokens(string:trim(Output), " \t"),
    SizeHex = lists:nth(3, Fields),
    % Ensure we got it right
    SectionSize = list_to_integer(SizeHex, 16),
    OffsetHex = lists:nth(6, Fields),
    Offset = list_to_integer(OffsetHex, 16),

    % Write new info size
    AVMInfoTempFile = Executable ++ ".atomvm_avm_info",
    ok = file:write_file(AVMInfoTempFile, <<Offset:64/little, SectionSize:64/little>>),

    ObjCopyCmd = lists:flatten(
        io_lib:format(
            "~s --update-section .atomvm_avm_info=~s --set-section-flags .atomvm_avm_info=alloc,load,readonly,data ~s",
            [ObjCopyTool, AVMInfoTempFile, Executable]
        )
    ),
    rebar_api:debug("Step 3 - Replace info section: ~s", [ObjCopyCmd]),
    ok = run_objcopy_cmd(ObjCopyCmd),
    ok = file:delete(AVMInfoTempFile).

%% @private
make_executable(Path) ->
    rebar_api:debug("Making ~s executable", [Path]),
    case file:read_file_info(Path) of
        {ok, FileInfo} ->
            NewMode = FileInfo#file_info.mode bor 8#00111,
            case file:write_file_info(Path, FileInfo#file_info{mode = NewMode}) of
                ok -> ok;
                {error, Reason} -> throw({chmod_failed, Path, Reason})
            end;
        {error, Reason} ->
            throw({stat_failed, Path, Reason})
    end.
