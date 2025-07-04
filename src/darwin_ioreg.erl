%
% This file is part of atomvm_rebar3_plugin.
%
% Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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

-module(darwin_ioreg).

-export([
    pico_callout_devices/0
]).

% Parse ioreg output to find out the callout devices attached to Pico.
% No pico connected:
% 1> darwin_ioreg:pico_callout_devices().
% []
% One pico connected:
% 2> darwin_ioreg:pico_callout_devices().
% [<<"/dev/cu.usbmodem14401">>]
% Two picos connected:
% 3> darwin_ioreg:pico_callout_devices().
% [<<"/dev/cu.usbmodem14401">>,<<"/dev/cu.usbmodem14301">>]
-spec pico_callout_devices() -> [unicode:unicode_binary()].
pico_callout_devices() ->
    ResultStr = os:cmd("ioreg -r -n IOSerialBSDClient -t"),
    ResultBin = list_to_binary(ResultStr),
    ResultLines = binary:split(ResultBin, <<"\n">>, [global]),
    parse(ResultLines, []).

parse([<<"+-o Root  ", _/binary>> | T], Acc) ->
    parse_tree(T, root, Acc);
parse([<<" ", Rest/binary>> | T], Acc) ->
    parse([Rest | T], Acc);
parse([<<>> | T], Acc) ->
    parse(T, Acc);
parse([], Acc) ->
    Acc.

parse_tree([<<" ", Rest/binary>> | T], IsPico, Acc) ->
    parse_tree([Rest | T], IsPico, Acc);
parse_tree([<<"+-o Pico@", _/binary>> | T], root, Acc) ->
    parse_tree(T, true, Acc);
parse_tree([<<"+-o IOSerialBSDClient ", _/binary>> | T], IsPico, Acc) ->
    parse_ioserialbsdclient(T, IsPico, Acc);
parse_tree([<<"+-o ", _/binary>> | T], IsPico, Acc) ->
    parse_tree(T, IsPico, Acc).

parse_ioserialbsdclient([<<" ", Rest/binary>> | T], IsPico, Acc) ->
    parse_ioserialbsdclient([Rest | T], IsPico, Acc);
parse_ioserialbsdclient([<<"\"IOCalloutDevice\" = \"", Rest/binary>> | T], true, Acc) ->
    [CallOut | _] = binary:split(Rest, <<"\"">>),
    parse_ioserialbsdclient(T, true, [CallOut | Acc]);
parse_ioserialbsdclient([<<"}">> | T], _IsPico, Acc) ->
    parse(T, Acc);
parse_ioserialbsdclient([_Other | T], IsPico, Acc) ->
    parse_ioserialbsdclient(T, IsPico, Acc).
