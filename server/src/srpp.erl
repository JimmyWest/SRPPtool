-module(srpp).

-export([main/2]).

main(WD,Port) ->
    log:info(["SRPP server started!"]),
    log:info(["Reading files from ",WD]),
    log:info(["Listening on port ",Port]).

