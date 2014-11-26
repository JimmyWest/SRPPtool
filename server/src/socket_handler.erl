-module(socket_handler).

-export([start/2, response/3]).

start(Socket, Client) ->
    spawn(fun() -> init(Socket, Client) end).

response(Socket, error, Msg) ->
    ok;
response(Socket, update, Msg) ->
    ok;
response(Socket, Type, Msg) ->
    ok.

init(Socket, Client) ->
    log:info(["New connection initialized ..."]),
    % erlang:monitor(process,Client),
    recv_loop(Socket, Client).

recv_loop(Socket, Client) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, B} ->
	    Msg = parse_message(B),
	    client_handler:socket_message(Client, Socket, Msg),
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

parse_message(B) ->
    B.
