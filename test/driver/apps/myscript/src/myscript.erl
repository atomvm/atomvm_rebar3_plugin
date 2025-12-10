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
-module(myscript).

-export([main/1]).

-spec main(Args :: [string()]) -> 0 | ok | any().
main(Args) ->
    case Args of
        [] -> 0;
        ["error"] -> 1;
        Other -> io:format("Args = ~p\n", [Other])
    end.
