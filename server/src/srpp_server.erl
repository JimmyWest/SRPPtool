-module(srpp_server).

-export([start/3]).

-include("config.hrl").

start(WD,MainPort,FilePort) ->
    ?log_info(["SRPP server started!"]),
    ?log_info(["Reading files from ",WD]),
    ?log_info(["Main listening on port ",MainPort]),
    ?log_info(["File listening on port ",FilePort]),
    directory_handler:start(WD),
    port_handler:start(MainPort),
    port_handler:start(FilePort).
