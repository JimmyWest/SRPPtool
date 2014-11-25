-module(port_handler).

-export([start/1, stop/1]).

-include("config.hrl").

start(Port) ->
    spawn(fun() -> init(Port) end).

stop(Pid) ->
    common:send_sync(Pid, stop).

init(Port) ->
    log:info(["Port Handler started, initializing handler..."]),
    common:safe_register(socket_handler,self()),
    case gen_tcp:listen(Port, ?GEN_TCP_CONF) of
	{ok, ListenSocket} ->
	    log:info(["Starts listen on port ",Port," for new connections."]),
	    loop(ListenSocket);
	{error, Reason} ->
	    log:err(["Can't open listening socket on port ",Port,", due to: ",Reason])
    end.
loop(ListenSocket) ->
    {Msg, Com} = common:receive_msg(0),
    case Msg of
	stop ->
	    gen_tcp:close(ListenSocket),
	    common:reply(Com, ok),
	    log:info(["Port Handler closed on command!"]),
	    exit("Port Handler closed on command!");
	timeout ->
	    ok
    end,
    listen_loop(ListenSocket).

listen_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket, ?ACCEPT_TIMEOUT) of
	{ok, Socket} ->
	    socket_handler:start(Socket),
	    loop(ListenSocket);
	{error, Reason} ->
	    failure_recovery(Reason, ListenSocket)
    end.

failure_recovery(closed, _) ->
    log:info(["Socket closed, terminates listener in Port Handler"]),
    ok;
failure_recovery(timeout, ListenSocket) ->
    loop(ListenSocket);
failure_recovery(system_limit, ListenSocket) ->
    cooldown(ListenSocket);
failure_recovery(Reason, _) ->
    log:err(["Can't recover from listen socket error: ",Reason,", I will die now!"]).

cooldown(ListenSocket) ->
    log:info(["Socket listener cooldown due to overload of sockets."]),
    receive
    after ?SOCKET_COOLDOWN_TIMEOUT -> loop(ListenSocket)
    end.
