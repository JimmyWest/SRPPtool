-module(client_handler).

-export([start/1, socket_message/2, socket_closed/1, update/2]).

-include("config.hrl").

start(Socket) ->
    spawn(fun() -> init(Socket) end).

socket_message(Client, Msg) ->
    common:send(Client, {socket, Msg}).

socket_closed(Client) ->
    common:send(Client, {socket, closed}).

update(Client, Change) ->
    common:send(Client, {update, Change}).

init(Socket) ->
    ?log_start(["Started!"]),
    socket_handler:start(Socket, self()),
    init_recv_loop(Socket).

init_recv_loop(Socket) ->
    {Msg, _} = common:receive_msg(unlimited),
    ?log_debug(["Got msg: ",Msg]),
    case Msg of
	{socket, closed} ->
	    ok;
	{socket, SocketMsg} ->
	    init_handle_socket_message(Socket, SocketMsg)
    end.

init_handle_socket_message(Socket, Msg) ->
    case Msg of
	{openfile, Id, FileId} ->
	    openfile(Socket, Id, FileId);
	Wierd ->
	    ?log_wierd([Wierd]),
	    init_recv_loop(Socket)
    end.

openfile(Socket, Id, FileId) ->
    case directory_handler:subscribe(FileId) of
	{error, Reason} ->
	    ?log_error(["Can't subscribe to file(",FileId,"), due to: ",Reason]),
	    socket_handler:response(Socket, error, {Id, "Can't open file. Retry again ..."}),
	    init_recv_loop(Socket);
	Controller ->
	    recv_loop(Socket, Controller, FileId)
    end.

recv_loop(Socket, Controller, ID) ->
    {Msg, _} = common:receive_msg(unlimited),
    ?log_debug(["Got msg: ",Msg]),
    case Msg of
	{socket, Msg} ->
	    handle_socket_message(Controller, Msg),
	    recv_loop(Socket, Controller, ID);
	{socket, closed} ->
	    ?log_info(["Client handler terminate as socket is closed"]),
	    file_controller:unsubscribe(Controller, self()),
	    ok;
	{update, Change} ->
	    socket_handler:response(Socket, update, Change),
	    recv_loop(Socket, Controller, ID);
	Wierd ->
	    ?log_wierd([Wierd]),
	    recv_loop(Socket, Controller, ID)
    end.

handle_socket_message(Controller, Msg) ->
    case Msg of
	{add, N, Line} ->
	    file_controller:add_line(Controller, N, Line);
	{update, N, Line} ->
	    file_controller:update_line(Controller, N, Line);
	{remove, N} ->
	    file_controller:remove_line(Controller, N);
	{get, N} ->
	    file_controller:get_line(Controller, N);
	{save} ->
	    file_controller:save(Controller);
	{close} ->
	    file_controller:close(Controller)
    end.
