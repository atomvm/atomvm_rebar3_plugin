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
-module(atomvm_rebar3_plugin).

-export([init/1]).

%% internal API
-export([proplist_to_map/1]).

-define(PROVIDERS, [
    atomvm_packbeam_provider,
    atomvm_esp32_flash_provider,
    atomvm_pico_flash_provider,
    atomvm_stm32_flash_provider,
    atomvm_uf2create_provider,
    legacy_packbeam_provider,
    legacy_esp32_flash_provider,
    legacy_stm32_flash_provider
]).

-type proplist() :: [{term(), term()} | term()].

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    lists:foldl(
        fun(Cmd, {ok, StateAcc}) ->
            apply(Cmd, init, [StateAcc])
        end,
        {ok, State},
        ?PROVIDERS
    ).

-spec proplist_to_map(Proplist :: proplist()) -> map().
proplist_to_map(Proplist) ->
    proplist_to_map(Proplist, #{}).

%% @private
proplist_to_map([], Accum) ->
    Accum;
proplist_to_map([{K, V} | T], Accum) ->
    proplist_to_map(T, Accum#{K => V});
proplist_to_map([K | T], Accum) ->
    proplist_to_map(T, Accum#{K => true}).
