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

-export([read_app_offset/4]).

-define(HEADER, <<16#aa, 16#50>>).
-define(PART_END, <<0, 0, 0, 0>>).
-define(DATA_PARTITION, <<16#01>>).
-define(PHY_SUBTYPE, <<16#01>>).
-define(AVM_APP_SUBTYPE, <<16#aa>>).

-spec read_app_offset(
    Esptool :: string(), Port :: string(), PartName :: binary(), TempFile :: string()
) -> Address :: integer().
read_app_offset(Esptool, Port, PartName, TempFile) ->
    Partitions = partitions_from_dev(Esptool, Port, TempFile),
    Partition_data = find_partition(PartName, Partitions),
    Header = ?HEADER,
    End = ?PART_END,
    Address =
        case Partition_data of
            <<Header:2/binary, _TypeInfo:2/binary, 00, _Address:4/binary, 00, _Options:2/binary,
                _NameData:16/binary, End:4/binary>> ->
                parse_offset(PartName, Partition_data);
            _ ->
                error(invalid_partition_table)
        end,
    binary:decode_unsigned(Address, big).

%% @private
-spec partitions_from_dev(Esptool :: string(), Port :: string(), TempFile :: string()) ->
    PartitionData :: binary().
partitions_from_dev(Esptool, Port, TempFile) ->
    PortOpt =
        case Port of
            "auto" -> "";
            _ -> lists:flatten(["--port ", Port])
        end,
    Cmd = lists:join(" ", [
        Esptool,
        PortOpt,
        "read_flash",
        "0x8000",
        "0xc00",
        TempFile
    ]),
    os:cmd(Cmd),
    Partition_data =
        case file:read_file(TempFile) of
            {ok, Data} ->
                Data;
            {error, enoent} ->
                error(no_device);
            Error ->
                error(Error)
        end,
    Partition_data.

%% @private
%% <<PartitionTable:3072/binary>> -> <<Partition_data:32/binary>>
-spec find_partition(PartName :: binary(), PartitionTable :: binary()) -> PartitionData :: binary().
find_partition(PartName, <<Partition:32/binary, Partitions/binary>>) ->
    Header = ?HEADER,
    End = ?PART_END,
    NameData =
        case <<Partition:32/binary>> of
            <<Header:2/binary, _TypeInfo:2/binary, 00, _Addr:4/binary, 00, _Options:2/binary,
                PName:16/binary, End:4/binary>> ->
                PName;
            _ ->
                <<>>
        end,
    [Name | _End] = binary:split(NameData, <<0>>),
    case Name of
        PartName ->
            <<Partition:32/binary>>;
        _NoMatch ->
            find_partition(PartName, Partitions)
    end;
find_partition(PartName, <<>>) ->
    error({partition_not_found, PartName}).

%% @private
%% <<Partition_data:32/binary>> -> <<Address:4/binary>>
-spec parse_offset(Name :: binary(), Partition_data :: binary()) -> Address :: binary().
parse_offset(Name, Partition_data) ->
    Header = ?HEADER,
    End = ?PART_END,
    <<Header:2/binary, Type:1/binary, Subtype:1/binary, 00, Address:4/binary, 00, _Options:2/binary,
        NameData:16/binary, End:4/binary>> = Partition_data,
    [NameBin | _End] = binary:split(NameData, <<0>>),
    Offset =
        case {NameBin, <<Type:1/binary>>, <<Subtype:1/binary>>} of
            {Name, ?DATA_PARTITION, ?PHY_SUBTYPE} ->
                Address;
            {Name, ?DATA_PARTITION, ?AVM_APP_SUBTYPE} ->
                Address;
            {Name, ?DATA_PARTITION, InvalidType} ->
                error({invalid_subtype, binary:encode_hex(InvalidType)});
            _NotFound ->
                error(invalid_partition_data)
        end,
    Offset.
