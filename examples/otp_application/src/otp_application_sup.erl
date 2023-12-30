%%
%% Copyright (c) 2023 <fred@dushin.net>
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
-module(otp_application_sup).

-export([start/1, init/1]).

start(Args) ->
    io:format("Starting otp_application_sup with args ~p ...~n", [Args]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%%
%% supervisor implementation
%%

init(Args) ->
    {ok, {
        {one_for_one, 1, 1}, [
            {
                otp_application_worker,
                {otp_application_worker, start_link, [Args]},
                permanent,
                brutal_kill,
                worker,
                []
            }
        ]
    }
}.
