%%
%% Copyright (c) 2023 Uncle Grumpy <winford@object.stream>
%% All rights reserved.
%%
%% Based on esp32_flash_provider.erl
%% Copyright (c) dushin.net
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
-module(legacy_stm32_flash_provider).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, stm32_flash).
-define(DEPS, [packbeam]).
-define(OPTS, [
    {stflash, $s, "stflash", string, "Path to st-flash"},
    {offset, $o, "offset", string, "Offset (default 0x8080000)"}
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
        {example, "rebar3 stm32_flash"},
        % list of options understood by the plugin
        {opts, ?OPTS},
        {short_desc, "A rebar plugin to flash packbeam files to STM32 devices (DEPRECATED)"},
        {desc,
            "A rebar plugin to flash packbeam files to STM32 devices.~n~n"
            "IMPORTANT! this plugin has been DEPRECATED and will be REMOVED in the 0.9.0 release!~n"
            "Use `rebar3 atomvm stm32_flash`, instead.~n"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:warn("DEPRECATED The stm32_flash tool has been moved under the atomvm namespace", []),
    rebar_api:warn("This legacy provider will be REMOVED in the 0.9.0 release.", []),
    atomvm_stm32_flash_provider:do(State).

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    atomvm_stm32_flash_provider:format_error(Reason).
