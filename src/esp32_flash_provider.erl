%%
%% Copyright (c) dushin.net
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     * Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     * Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     * Neither the name of dushin.net nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY dushin.net ``AS IS'' AND ANY
%% EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%% DISCLAIMED. IN NO EVENT SHALL dushin.net BE LIABLE FOR ANY
%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%
-module(esp32_flash_provider).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-include_lib("kernel/include/file.hrl").

-define(PROVIDER, esp32_flash).
-define(DEPS, [packbeam]).
-define(OPTS, [
    {esptool, $e, "esptool", undefined, "Path to esptool.py"},
    {port, $p, "port", undefined, "Device port (default /dev/ttyUSB0)"},
    {baud, $b, "baud", undefined, "Baud rate (default 115200)"},
    {offset, $o, "offset", undefined, "Offset (default 0x110000)"}
]).

%%
%% provider implementation
%%
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 esp32_flash"}, % How to use the plugin
            {opts, ?OPTS},                   % list of options understood by the plugin
            {short_desc, "A rebar plugin to flash packbeam to ESP32 devices"},
            {desc, "A rebar plugin to flash packbeam to ESP32 devices"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case parse_args(rebar_state:command_args(State)) of
        {ok, Opts} ->
            do_flash(
                rebar_state:project_apps(State),
                maps:get(
                    esptool, Opts,
                    os:getenv(
                        "ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_ESPTOOL",
                        "esptool.py"
                    )
                ),
                maps:get(port, Opts, os:getenv("ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_PORT", "/dev/ttyUSB0")),
                maps:get(baud, Opts, os:getenv("ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_BAUD", "115200")),
                maps:get(offset, Opts, os:getenv("ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_OFFSET", "0x110000"))
            ),
            {ok, State};
        {error, Reason} ->
            io:format("~p~n", [Reason]),
            {error, Reason}
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%
%% internal functions
%%

%% @private
parse_args(Args) ->
    parse_args(Args, #{external_avms => [], prune => false, force => false}).

%% @private
parse_args([], Accum) ->
    {ok, Accum};
parse_args(["-e", EspTool|Rest], Accum) ->
    parse_args(Rest, Accum#{esptool => EspTool});
parse_args(["--esptool", EspTool|Rest], Accum) ->
    parse_args(Rest, Accum#{esptool => EspTool});
parse_args(["-p", Port|Rest], Accum) ->
    parse_args(Rest, Accum#{port => Port});
parse_args(["--port", Port|Rest], Accum) ->
    parse_args(Rest, Accum#{port => Port});
parse_args(["-b", Baud|Rest], Accum) ->
    parse_args(Rest, Accum#{baud => Baud});
parse_args(["--baud", Baud|Rest], Accum) ->
    parse_args(Rest, Accum#{baud => Baud});
parse_args(["-o", Offset|Rest], Accum) ->
    parse_args(Rest, Accum#{offset => Offset});
parse_args(["--offset", Offset|Rest], Accum) ->
    parse_args(Rest, Accum#{offset => Offset});
parse_args([_|Rest], Accum) ->
    parse_args(Rest, Accum).



%% @private
do_flash(ProjectApps, EspTool, Port, Baud, Offset) ->
    [ProjectAppAVM|_] = [get_avm_file(ProjectApp) || ProjectApp <- ProjectApps],
    Cmd = lists:join(" ", [
            EspTool,
            "--chip", "esp32",
            "--port", Port,
            "--baud", Baud,
            "--before", "default_reset",
            "--after", "hard_reset",
            "write_flash",
            "-u",
            "--flash_mode", "dio",
            "--flash_freq", "40m",
            "--flash_size", "detect",
            Offset,
            ProjectAppAVM
        ]),
    try
        rebar_api:info("~s~n", [Cmd]),
        io:format("~s", [os:cmd(Cmd)])
    catch
        _:Error ->
             rebar_api:error("Error executing ~p.  Error: ~p", [Cmd, Error])
    end.

%% @private
get_avm_file(App) ->
    OutDir = rebar_app_info:out_dir(App),
    Name = binary_to_list(rebar_app_info:name(App)),
    DirName = filename:dirname(OutDir),
    filename:join(DirName, Name ++ ".avm").
