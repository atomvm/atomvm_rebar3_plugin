-module(init_shim).

-export([start/0]).

start() ->
    init:boot().
