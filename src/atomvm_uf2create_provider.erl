%
% This file is part of AtomVM.
%
% Copyright 2022 Paul Guyot <pguyot@kallisys.net>
%
% Adapted for atomvm_rebar3_plugin:
% Copyright 2023 Winford (UncleGrumpy) <winford@object.stream>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(atomvm_uf2create_provider).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-include_lib("kernel/include/file.hrl").

-define(PROVIDER, uf2create).
-define(DEPS, [packbeam]).
-define(OPTS, [
    {output, $o, "output", string, "Output path/name"},
    {start, $s, "start", string, "Start address for the uf2 binary (default 0x10180000)"},
    {input, $i, "input", string, "Input avm file to convert to uf2"}
]).

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
        {example, "rebar3 atomvm uf2create"},
        % list of options understood by the plugin
        {opts, ?OPTS},
        {short_desc, "A rebar plugin to create uf2 files"},
        {desc, "A rebar plugin to create uf2 files from packbeam files"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    try
        Opts = get_opts(rebar_state:command_parsed_args(State)),
        OutFile = get_out_file(State),
        TargetAVM = get_avm_file(State),
        ok = do_uf2create(
            maps:get(output, Opts, OutFile),
            parse_addr(maps:get(start, Opts, "0x10180000")),
            maps:get(input, Opts, TargetAVM)
        ),
        {ok, State}
    catch
        _:E ->
            rebar_api:error("~p~n", [E]),
            {error, E}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).


%%
%% internal functions
%%

%% @private
get_opts({ParsedArgs, _}) ->
    atomvm_rebar3_plugin:proplist_to_map(ParsedArgs).

%% @private
get_avm_file(State) ->
    [App] = [ProjectApp || ProjectApp <- rebar_state:project_apps(State)],
    OutDir = rebar_app_info:out_dir(App),
    Name = binary_to_list(rebar_app_info:name(App)),
    DirName = filename:dirname(OutDir),
    filename:join(DirName, Name ++ ".avm").

%% @private
get_out_file(State) ->
    [App] = [ProjectApp || ProjectApp <- rebar_state:project_apps(State)],
    OutDir = rebar_app_info:out_dir(App),
    Name = binary_to_list(rebar_app_info:name(App)),
    DirName = filename:dirname(OutDir),
    filename:join(DirName, Name ++ ".uf2").

%% @private
parse_addr("0x" ++ AddrHex) ->
    list_to_integer(AddrHex, 16);
parse_addr("16#" ++ AddrHex) ->
    list_to_integer(AddrHex, 16);
parse_addr(AddrDec) ->
    list_to_integer(AddrDec).

%%% UF2 defines
-define(UF2_MAGIC_START0, 16#0A324655).
-define(UF2_MAGIC_START1, 16#9E5D5157).
-define(UF2_MAGIC_END, 16#0AB16F30).

-define(UF2_FLAG_FAMILY_ID_PRESENT, 16#00002000).

%%% Pico defines
-define(UF2_PICO_FLAGS, ?UF2_FLAG_FAMILY_ID_PRESENT).
-define(UF2_PICO_PAGE_SIZE, 256).
-define(UF2_PICO_FAMILY_ID, 16#E48BFF56).

%%%

do_uf2create(OutputPath, StartAddr, ImagePath) ->
    {ok, ImageBin} = file:read_file(ImagePath),
    BlocksCount0 = byte_size(ImageBin) div ?UF2_PICO_PAGE_SIZE,
    BlocksCount =
        BlocksCount0 +
            if
                byte_size(ImageBin) rem ?UF2_PICO_PAGE_SIZE =:= 0 -> 0;
                true -> 1
            end,
    OutputBin = uf2create0(0, BlocksCount, StartAddr, ImageBin, []),
    ok = file:write_file(OutputPath, OutputBin).

%% @private
uf2create0(_BlockIndex, _BlocksCount, _BaseAddr, <<>>, Acc) ->
    lists:reverse(Acc);
uf2create0(BlockIndex, BlocksCount, BaseAddr, ImageBin, Acc) ->
    {PageBin, Tail} =
        if
            byte_size(ImageBin) >= ?UF2_PICO_PAGE_SIZE ->
                split_binary(ImageBin, ?UF2_PICO_PAGE_SIZE);
            true ->
                {ImageBin, <<>>}
        end,
    PaddedData = pad_binary(PageBin, 476),
    Block = [
        <<
            ?UF2_MAGIC_START0:32/little,
            ?UF2_MAGIC_START1:32/little,
            ?UF2_PICO_FLAGS:32/little,
            BaseAddr:32/little,
            ?UF2_PICO_PAGE_SIZE:32/little,
            BlockIndex:32/little,
            BlocksCount:32/little,
            ?UF2_PICO_FAMILY_ID:32/little
        >>,
        PaddedData,
        <<?UF2_MAGIC_END:32/little>>
    ],
    uf2create0(BlockIndex + 1, BlocksCount, BaseAddr + ?UF2_PICO_PAGE_SIZE, Tail, [Block | Acc]).

%% @private
pad_binary(Bin, Len) ->
    PadCount = Len - byte_size(Bin),
    Pad = binary:copy(<<0>>, PadCount),
    [Bin, Pad].
