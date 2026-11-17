%%
%% Copyright (c) 2025 Winford (UncleGrumpy) <winford@object.stream>
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
-module(esp_part_dump).

-export([get_app_offset/7, get_app_offset/2, partition_at_offset/7, partition_at_offset/2]).

-type fs_type() ::
    phy
    | avm_app
    | undefined
    | {data, SubType :: byte()}
    | protected
    | {reserved, ota | nvs | coredump | nvs_keys | fat | efuse | spiffs | littlefs}.

-spec get_app_offset(
    Esptool :: string(),
    Chip :: string(),
    Port :: string(),
    Baud :: non_neg_integer(),
    PartName :: string(),
    TempFile :: string(),
    LogFile :: file:name_all()
) -> Offset :: non_neg_integer().
get_app_offset(Esptool, Chip, Port, Baud, PartName, TempFile, LogFile) ->
    Partitions = get_partition_data(Esptool, Chip, Port, Baud, TempFile, LogFile),
    lookup_partition_by_name(PartName, Partitions).

-spec get_app_offset(
    PartName :: string(), File :: string()
) -> Offset :: non_neg_integer().

get_app_offset(PartName, File) ->
    Partitions =
        case file:read_file(File) of
            {ok, Data} ->
                Data;
            {error, enoent} ->
                error(no_dump_file)
        end,
    lookup_partition_by_name(PartName, Partitions).

-spec lookup_partition_by_name(Name :: string(), Partitions :: binary()) ->
    Offset :: non_neg_integer().
lookup_partition_by_name(Name, Partitions) ->
    PartitionsData = parse_data_partitions(Partitions),
    case lists:keyfind(Name, 2, PartitionsData) of
        false ->
            error({partition_not_found, Name});
        {_Offset, Name, protected} ->
            error({invalid_partition_type, Name});
        {_Offset, Name, {reserved, SubType}} ->
            error({invalid_subtype, {Name, SubType}});
        {Offset, _Name, _SubType} ->
            Offset
    end.

-spec partition_at_offset(
    Esptool :: string(),
    Chip :: string(),
    Port :: string(),
    Baud :: non_neg_integer(),
    Offset :: non_neg_integer(),
    TempFile :: file:name_all(),
    LogFile :: file:name_all()
) ->
    {Name :: string(), Type :: fs_type()}.
partition_at_offset(Esptool, Chip, Port, Baud, Offset, TempFile, LogFile) ->
    Partitions = get_partition_data(Esptool, Chip, Port, Baud, TempFile, LogFile),
    lookup_partition_by_offset(Offset, Partitions).

-spec partition_at_offset(Offset :: non_neg_integer(), TempFile :: file:name_all()) ->
    {Name :: string(), Type :: fs_type()}.
partition_at_offset(Offset, File) ->
    Partitions =
        case file:read_file(File) of
            {ok, Data} ->
                Data;
            {error, enoent} ->
                error(no_dump_file)
        end,
    lookup_partition_by_offset(Offset, Partitions).

% @private
-spec lookup_partition_by_offset(Offset :: non_neg_integer(), Partitions :: binary()) ->
    {Name :: string(), Type :: fs_type()}.
lookup_partition_by_offset(Offset, Partitions) ->
    PartitionsData = parse_data_partitions(Partitions),
    case lists:keyfind(Offset, 1, PartitionsData) of
        false ->
            error({invalid_partition, {Offset, "none", "no partition aligned to address"}});
        {Offset, Name, protected} ->
            error({invalid_partition, {Offset, Name, "not a data partition"}});
        {Offset, Name, {reserved, Type}} ->
            Reason = io_lib:format("partition reserved for ~s", [Type]),
            error({invalid_partition, {Offset, Name, Reason}});
        {Offset, Name, Type} ->
            {Name, Type}
    end.

% @private
-spec get_partition_data(
    Esptool :: string(),
    Chip :: string(),
    Port :: string(),
    Baud :: non_neg_integer(),
    TempFile :: file:name_all(),
    LogFile :: file:name_all()
) ->
    PartitionData :: binary().
get_partition_data(Esptool, Chip, Port, Baud, TempFile, LogFile) ->
    case file:read_file(TempFile) of
        {ok, Data} ->
            Data;
        {error, _} ->
            dump_partition_table(Esptool, Chip, Port, Baud, TempFile, LogFile)
    end.

%% @private
-spec dump_partition_table(
    Esptool :: string(),
    Chip :: string(),
    Port :: string(),
    Baud :: non_neg_integer(),
    TempFile :: string(),
    Logfile :: file:name_all()
) ->
    PartitionData :: binary().
dump_partition_table(Esptool, Chip, Port, Baud, TempFile, Logfile) ->
    BaseArgs = [
        "--chip",
        Chip,
        "--baud",
        integer_to_list(Baud),
        "read_flash",
        "0x8000",
        "0xC00",
        TempFile
    ],
    Args =
        case Port of
            "auto" ->
                BaseArgs;
            _ ->
                ["--port", Port | BaseArgs]
        end,
    %% The following log output is parsed by the tests and should not be changed or removed.
    case os:getenv("TEST") of
        false ->
            ok;
        _ ->
            rebar_api:info("~s ~s", [Esptool, lists:flatten(lists:join(" ", Args))])
    end,

    ok = atomvm_rebar3_plugin:external_command(Esptool, Args, Logfile),

    PartitionData =
        case file:read_file(TempFile) of
            {ok, Data} ->
                Data;
            {error, enoent} ->
                error(no_device_dump);
            Error ->
                error({file_read, Error})
        end,
    PartitionData.

%% @private
-spec parse_data_partitions(PartitionTable :: binary()) ->
    [
        {
            Offset :: non_neg_integer(),
            Name :: string(),
            Type :: fs_type()
        }
    ].
parse_data_partitions(PartitionTable) ->
    parse_data_partitions(PartitionTable, []).

parse_data_partitions(<<>>, Acc) ->
    lists:reverse(Acc);
parse_data_partitions(
    <<16#AA, 16#50, Type:8, SubType:8, Offset:32/little, _Size:32/little, PName:16/binary,
        _Flags:32/little, Rest/binary>>,
    Acc
) ->
    [Name | _] = binary:split(PName, <<0>>),
    Entry = {Offset, binary_to_list(Name), classify(Type, SubType)},
    parse_data_partitions(Rest, [Entry | Acc]);
parse_data_partitions(
    <<16#EB, 16#EB, _Padding:14/binary, _MD5:16/binary, _Rest/binary>>, Acc
) ->
    %% Reached end of partition table marker.
    lists:reverse(Acc);
parse_data_partitions(<<16#FF, _/binary>>, Acc) ->
    %% Erased flash beyond end of partition table marker.
    lists:reverse(Acc);
parse_data_partitions(_, _) ->
    error(corrupt_partition_table).

classify(16#01, 16#00) -> {reserved, ota};
classify(16#01, 16#01) -> phy;
classify(16#01, 16#02) -> {reserved, nvs};
classify(16#01, 16#03) -> {reserved, coredump};
classify(16#01, 16#04) -> {reserved, nvs_keys};
classify(16#01, 16#05) -> {reserved, efuse};
classify(16#01, 16#06) -> undefined;
classify(16#01, 16#81) -> {reserved, fat};
classify(16#01, 16#82) -> {reserved, spiffs};
classify(16#01, 16#83) -> {reserved, littlefs};
classify(16#01, 16#AA) -> avm_app;
%% any other data subtype
classify(16#01, Sub) -> {data, Sub};
%% non-data partition (type != 0x01)
classify(_, _) -> protected.
