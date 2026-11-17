%%
%% Copyright (c) 2026 UncleGrumpy <winford@object.stream>
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
-module(esp_part_dump_tests).

-export([run/1]).

run(Opts) ->
    ok = test_parse_partitions(Opts),
    ok = test_retention_policy().

%% @private
test_parse_partitions(_Opts) ->
    PartitionDump = os:getenv("ATOMVM_REBAR3_PLUGIN_ESP32_PARTITION_DUMP"),
    {ok, PartitionTable} = file:read_file(PartitionDump),

    ok = parse_data_partitions_test(
        PartitionTable,
        [
            {16#9000, "nvs", {reserved, nvs}},
            {16#10000, "factory", protected},
            {16#1D0000, "boot.avm", phy}
        ]
    ),
    ok.

%% @private
parse_data_partitions_test(PartitionTable, ExpectResults) ->
    PartitionsInfo = esp_part_dump:parse_data_partitions(PartitionTable),
    expect_partitions(ExpectResults, PartitionsInfo, []).

expect_partitions([], _Output, Results) ->
    case lists:member(false, Results) of
        true ->
            {error, "unexpected partition PartitionTable results"};
        false ->
            ok
    end;
expect_partitions([Expect | ExpectResults], Output, Results) ->
    Result =
        case lists:member(Expect, Output) of
            true ->
                test:tick(),
                true;
            false ->
                false
        end,
    expect_partitions(ExpectResults, Output, [Result | Results]).

%% The flash provider classifies a run outcome into an artifact retention policy.
%% These are the cases the integration tests cannot reach: a corrupt dump and an
%% unanticipated failure must both keep their artifacts for debugging.
%% @private
test_retention_policy() ->
    Provider = atomvm_esp32_flash_provider,

    %% success leaves nothing
    {success, none} = Provider:artifact_policy(success),

    %% the tool failed: keep the log, the dump is useless
    {tool_failure, log} = Provider:artifact_policy({error, {"esptool", {exit_status, 2}, "l"}}),
    {tool_failure, log} = Provider:artifact_policy(
        {error, {timeout, "esptool", over_2_minutes}}
    ),
    {tool_failure, log} = Provider:artifact_policy({error, {enoent, "esptool"}}),
    {tool_failure, log} = Provider:artifact_policy({error, {eacces, "esptool"}}),

    %% a dump that was read but is unusable: keep the log and the dump
    {dump_lookup, log_dump} = Provider:artifact_policy({error, {partition_not_found, "fake"}}),
    {dump_lookup, log_dump} = Provider:artifact_policy({error, {invalid_subtype, {"bad1", fat}}}),
    {dump_lookup, log_dump} = Provider:artifact_policy(
        {error, {invalid_partition_type, "bad2"}}
    ),
    {dump_lookup, log_dump} = Provider:artifact_policy(
        {error, {invalid_partition, {16#210000, "app1.avm", "not a data partition"}}}
    ),

    %% a dump that could not be produced or parsed is never anticipated
    {dump_corrupt, log_dump} = Provider:artifact_policy({error, corrupt_partition_table}),
    {dump_corrupt, log_dump} = Provider:artifact_policy({error, {file_read, eacces}}),
    {dump_failure, log_dump} = Provider:artifact_policy({error, no_device_dump}),
    {dump_failure, log_dump} = Provider:artifact_policy({error, no_dump_file}),

    %% an unanticipated failure keeps everything
    {spawn_failure, log_dump} = Provider:artifact_policy(
        {error, {"esptool", ["--chip", "esp32"], error, eacces, []}}
    ),
    {unexpected, log_dump} = Provider:artifact_policy({error, {badmatch, 42}}),

    %% only the anticipated causes are excused in test mode
    true = Provider:anticipated(tool_failure),
    true = Provider:anticipated(dump_lookup),
    false = Provider:anticipated(dump_failure),
    false = Provider:anticipated(dump_corrupt),
    false = Provider:anticipated(unexpected),
    false = Provider:anticipated(spawn_failure),
    false = Provider:anticipated(success),

    test:tick(),
    ok.
