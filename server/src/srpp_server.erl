-module(srpp_server).

-export([start/2]).

start(WD,Port) ->
    log:info(["SRPP server started!"]),
    log:info(["Reading files from ",WD]),
    log:info(["Listening on port ",Port]).

