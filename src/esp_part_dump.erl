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

-export([read_app_offset/5, read_app_offset/2, partition_at_offset/5, partition_at_offset/2]).

-type fs_type() ::
    phy
    | avm_app
    | undefined
    | byte()
    | protected
    | {reserved, ota | nvs | coredump | nvs_keys | fat | efuse | spiffs | littlefs}.

-spec read_app_offset(
    Esptool :: string(),
    Port :: string(),
    PartName :: string(),
    TempFile :: string(),
    LogFile :: file:name_all()
) -> Offset :: non_neg_integer().
read_app_offset(Esptool, Port, PartName, TempFile, LogFile) ->
    Partitions = get_partition_data(Esptool, Port, TempFile, LogFile),
    Offset = lookup_partition_by_name(PartName, Partitions),
    Offset.

-spec read_app_offset(
    PartName :: string(), File :: string()
) -> Offset :: non_neg_integer().

read_app_offset(PartName, File) ->
    Partitions =
        case file:read_file(File) of
            {ok, Data} ->
                Data;
            {error, enoent} ->
                error(no_dump_file)
        end,
    Offset = lookup_partition_by_name(PartName, Partitions),
    Offset.

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
    Port :: string(),
    Offset :: non_neg_integer(),
    TempFile :: file:name_all(),
    LogFile :: file:name_all()
) ->
    {Name :: string(), Type :: fs_type()}.
partition_at_offset(Esptool, Port, Offset, TempFile, LogFile) ->
    Partitions = get_partition_data(Esptool, Port, TempFile, LogFile),
    Partition = lookup_partition_by_offset(Offset, Partitions),
    Partition.

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
    Partition = lookup_partition_by_offset(Offset, Partitions),
    Partition.

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
    Esptool :: string(), Port :: string(), TempFile :: file:name_all(), LogFile :: file:name_all()
) ->
    PartitionData :: binary().
get_partition_data(Esptool, Port, TempFile, LogFile) ->
    case file:read_file(TempFile) of
        {ok, Data} ->
            Data;
        {error, _} ->
            dump_device_partition(Esptool, Port, TempFile, LogFile)
    end.

%% @private
-spec dump_device_partition(
    Esptool :: string(), Port :: string(), TempFile :: string(), Logfile :: file:name_all()
) ->
    PartitionData :: binary().
dump_device_partition(Esptool, Port, TempFile, Logfile) ->
    BaseArgs = [
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
    case os:getenv("TEST") of
        false ->
            ok;
        _ ->
            rebar_api:info("~s ~s", [Esptool, lists:flatten(lists:join(" ", Args))])
    end,

    ok = atomvm_rebar3_plugin:external_command(Esptool, Args, Logfile),

    Partition_data =
        case file:read_file(TempFile) of
            {ok, Data} ->
                Data;
            {error, enoent} ->
                error(no_device_dump);
            Error ->
                error({file_read, Error})
        end,
    Partition_data.

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

parse_data_partitions(<<Partition:32/binary, Partitions/binary>>, PartitionData) ->
    case Partition of
        %% The default sub-type for AtomVM applications is phy
        <<16#aa50:16, 16#01, 16#01, 0, Address:4/binary, _:3/binary, PName:16/binary, 0:32>> ->
            [Name | _End] = binary:split(PName, <<0>>),
            parse_data_partitions(Partitions, [
                {binary:decode_unsigned(Address), binary_to_list(Name), phy} | PartitionData
            ]);
        %% custom sub-type 0xAA can be used to recognize AtomVM applications
        <<16#aa50:16, 16#01, 16#aa, 0, Address:4/binary, _:3/binary, PName:16/binary, 0:32>> ->
            [Name | _End] = binary:split(PName, <<0>>),
            parse_data_partitions(Partitions, [
                {binary:decode_unsigned(Address), binary_to_list(Name), avm_app} | PartitionData
            ]);
        %% ESP-IDF sub-type "undefined" is allowed
        <<16#aa50:16, 16#01, 16#06, 0, Address:4/binary, _:3/binary, PName:16/binary, 0:32>> ->
            [Name | _End] = binary:split(PName, <<0>>),
            parse_data_partitions(Partitions, [
                {binary:decode_unsigned(Address), binary_to_list(Name), undefined} | PartitionData
            ]);
        %% Other data partition subtypes should not be used (including fat, nvs, coredump, efuse, etc...)
        <<16#aa50:16, 16#01, 16#00, 0, Address:4/binary, _:3/binary, PName:16/binary, 0:32>> ->
            [Name | _End] = binary:split(PName, <<0>>),
            parse_data_partitions(Partitions, [
                {binary:decode_unsigned(Address), binary_to_list(Name), {reserved, ota}}
                | PartitionData
            ]);
        <<16#aa50:16, 16#01, 16#02, 0, Address:4/binary, _:3/binary, PName:16/binary, 0:32>> ->
            [Name | _End] = binary:split(PName, <<0>>),
            parse_data_partitions(Partitions, [
                {binary:decode_unsigned(Address), binary_to_list(Name), {reserved, nvs}}
                | PartitionData
            ]);
        <<16#aa50:16, 16#01, 16#03, 0, Address:4/binary, _:3/binary, PName:16/binary, 0:32>> ->
            [Name | _End] = binary:split(PName, <<0>>),
            parse_data_partitions(Partitions, [
                {binary:decode_unsigned(Address), binary_to_list(Name), {reserved, coredump}}
                | PartitionData
            ]);
        <<16#aa50:16, 16#01, 16#04, 0, Address:4/binary, _:3/binary, PName:16/binary, 0:32>> ->
            [Name | _End] = binary:split(PName, <<0>>),
            parse_data_partitions(Partitions, [
                {binary:decode_unsigned(Address), binary_to_list(Name), {reserved, nvs_keys}}
                | PartitionData
            ]);
        <<16#aa50:16, 16#01, 16#05, 0, Address:4/binary, _:3/binary, PName:16/binary, 0:32>> ->
            [Name | _End] = binary:split(PName, <<0>>),
            parse_data_partitions(Partitions, [
                {binary:decode_unsigned(Address), binary_to_list(Name), {reserved, efuse}}
                | PartitionData
            ]);
        <<16#aa50:16, 16#01, 16#81, 0, Address:4/binary, _:3/binary, PName:16/binary, 0:32>> ->
            [Name | _End] = binary:split(PName, <<0>>),
            parse_data_partitions(Partitions, [
                {binary:decode_unsigned(Address), binary_to_list(Name), {reserved, fat}}
                | PartitionData
            ]);
        <<16#aa50:16, 16#01, 16#82, 0, Address:4/binary, _:3/binary, PName:16/binary, 0:32>> ->
            [Name | _End] = binary:split(PName, <<0>>),
            parse_data_partitions(Partitions, [
                {binary:decode_unsigned(Address), binary_to_list(Name), {reserved, spiffs}}
                | PartitionData
            ]);
        <<16#aa50:16, 16#01, 16#83, 0, Address:4/binary, _:3/binary, PName:16/binary, 0:32>> ->
            [Name | _End] = binary:split(PName, <<0>>),
            parse_data_partitions(Partitions, [
                {binary:decode_unsigned(Address), binary_to_list(Name), {reserved, littlefs}}
                | PartitionData
            ]);
        %% Catchall to allow any other sub-types
        <<16#aa50:16, 16#01, SubType:8, 0, Address:4/binary, _:3/binary, PName:16/binary, 0:32>> ->
            [Name | _End] = binary:split(PName, <<0>>),
            parse_data_partitions(Partitions, [
                {binary:decode_unsigned(Address), binary_to_list(Name), SubType} | PartitionData
            ]);
        %% Non data partitions are off-limits. BEAM applications should only be flashed to data partitions.
        <<16#aa50:16, _NonDatatype:3/binary, Address:4/binary, _:3/binary, PName:16/binary, 0:32>> ->
            [Name | _End] = binary:split(PName, <<0>>),
            parse_data_partitions(Partitions, [
                {binary:decode_unsigned(Address), binary_to_list(Name), protected} | PartitionData
            ]);
        %% End of table marker
        <<16#ebeb:16, 16#ffffffffffffffffffffffffffff:112, _:16/binary>> ->
            lists:reverse(PartitionData);
        _ ->
            error(corrupt_partition_data)
    end.
