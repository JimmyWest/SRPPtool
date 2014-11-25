-module(socket_handler).

-export([start/1]).

start(Socket) ->
    spawn(fun() -> init(Socket) end).

init(Socket) ->
    log:info(["New connection initialized ..."]),
    recv_loop(Socket).

recv_loop(Socket) ->
    ok.
