-module(socket_handler).

-export([start/1]).

start(Socket, Client) ->
    spawn(fun() -> init(Socket, Client) end).

init(Socket, Client) ->
    log:info(["New connection initialized ..."]),
    erlang:monitor(process,Client),
    recv_loop(Socket, Client).

recv_loop(Socket, Client) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, B} ->
	    Msg = parse_message(B),
	    client_handler:socket_message(Client, Msg),
	    recv_loop(Socket, Client);
	{error, Reason} ->
	    failure_recovery(Reason, Socket, Client)
    end.

failure_recovery(closed, _, Client) ->
    log:info(["Client(",Client,") socket closed!"]),
    client_handler:socket_closed(Client);
failure_recovery(Reason, Socket, Client) ->
    log:err(["Client(",Client,") socker error, due to: ",Reason]),
    client_handler:socket_closed(Client),
    gen_tcp:close(Socket).

