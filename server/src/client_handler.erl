-module(client_handler).

-export([start/1, socket_message/3, socket_closed/1]).

start(Socket) ->
    spawn(fun() -> init(Socket) end).

socket_message(Client, Socket, Msg) ->
    common:send(Client, {socket, Socket, Msg}).

socket_closed(Client) ->
    common:send(Client, {socket, closed}).

init(Socket) ->
    log:info(["Client Handler started!"]),
    Pid = socket_handler:start(Socket, self()),
    erlang:monitor(process, Pid),
    init_recv_loop().

init_recv_loop() ->
    {Msg, Com} = common:receive_msg(unlimited),
    log:debug(["Got msg: ",Msg]),
    case Msg of
	{socket, Socket, M} ->
	    handle_socket_message(Socket, M),
	    init_recv_loop();
	{socket, closed} ->
	    ok
    end.

handle_socket_message(Socket, Msg) ->
    case Msg of
	{openfile, Path, Filename} ->
	    ok;
	_ ->
	    ANSWER = "This is a test!",
	    gen_tcp:send(Socket,"HTTP/1.1 200 OK\r\nContent-Length: "++integer_to_list(length(ANSWER))++"\r\n\r\n"++ANSWER)
    end.
	    
	
