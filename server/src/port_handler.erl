-module(port_handler).

-export([start/1, stop/1]).

-include("config.hrl").

start(Port) ->
    spawn(fun() -> init(Port) end).

stop(Pid) ->
    common:send_sync(Pid, stop).

init([P]) when is_list(P) ->
    case string:to_integer(P) of
	{error, Reason} ->
	    ?log_error(["Parse port number error, due to ",Reason]);
	{Port, _} ->
	    init(Port)
    end;
init(Port) ->
    ?log_start(["Started, initializing handler for port:",Port," ..."]),
    common:safe_register(socket_handler,self()),
    case gen_tcp:listen(Port, ?GEN_TCP_CONF) of
	{ok, ListenSocket} ->
	    ?log_info(["Listening on port ",Port," for new connections."]),
	    loop(ListenSocket);
	{error, Reason} ->
	    ?log_error(["Can't open listening socket on port ",Port,", due to: ",Reason])
    end.
loop(ListenSocket) ->
    {Msg, Com} = common:receive_msg(0),
    case Msg of
	stop ->
	    gen_tcp:close(ListenSocket),
	    common:reply(Com, stopped),
	    ?log_info(["Port Handler closed on command!"]),
	    exit("Port Handler closed on command!");
	timeout ->
	    ok
    end,
    listen_loop(ListenSocket).

listen_loop(ListenSocket) ->
    ?log_heavydebug(["Listening for new accepts ..."]),
    case gen_tcp:accept(ListenSocket, ?ACCEPT_TIMEOUT) of
	{ok, Socket} ->
	    ?log_heavydebug(["New socket connection accepted by listener."]),
	    client_handler:start(Socket),
	    loop(ListenSocket);
	{error, Reason} ->
	    failure_recovery(Reason, ListenSocket)
    end.

failure_recovery(closed, _) ->
    ?log_info(["Socket closed, terminates listener in Port Handler"]),
    ok;
failure_recovery(timeout, ListenSocket) ->
    ?log_heavydebug(["gen_tcp:accept(ListenSocket) timedout, retries ..."]),
    loop(ListenSocket);
failure_recovery(system_limit, ListenSocket) ->
    cooldown(ListenSocket);
failure_recovery(Reason, _) ->
    ?log_error(["Can't recover from listen socket error: ",Reason,", I will die now!"]).

cooldown(ListenSocket) ->
    ?log_info(["Socket listener cooldown due to overload of sockets."]),
    receive
    after ?SOCKET_COOLDOWN_TIMEOUT -> loop(ListenSocket)
    end.
