-module(socket_handler).

-export([start/2, response/3, handle_message/3]).

-record(socket, {socket,keyring}).

-include("config.hrl").

start(Socket, Client) ->
    spawn(fun() -> init(Socket, Client) end).

response(Socket, error, Msg) ->
    ok;
response(Socket, update, Msg) ->
    ok;
response(Socket, Type, Msg) ->
    ok.

init(Socket, Client) ->
    ?log_start(["New connection initialized ..."]),
    KeyRing = crypt:new(),
    recv_loop(#socket{socket=Socket, keyring=KeyRing}, Client).

recv_loop(Socket, Client) ->
    case gen_tcp:recv(Socket#socket.socket, 0) of
	{ok, Data} ->
	    handle_message(Socket, Client, Data);
	{error, Reason} ->
	    failure_recovery(Reason, Socket, Client)
    end.

failure_recovery(closed, _, Client) ->
    ?log_info(["Client(",Client,") socket closed!"]),
    client_handler:socket_closed(Client);
failure_recovery(Reason, Socket, Client) ->
    ?log_error(["Client(",Client,") socker error, due to: ",Reason]),
    client_handler:socket_closed(Client),
    gen_tcp:close(Socket).

handle_message(Socket = #socket{keyring=KeyRing}, Client, Data) ->
    ?log_debug(["CipherText:\r\n",Data]),
    Data1 = crypt:decrypt(Data, KeyRing),
    ?log_debug(["PlainText:\r\n",Data1]),
    Actions = parse_tcp_data(Data1),
    {Socket1, Client1} = handle_action(Socket, Client, Actions),
    recv_loop(Socket1, Client1).

parse_tcp_data(Data) ->
    ?log_heavydebug(["Data\r\n",Data]),
    case Data of
	<<?TCP_HEAD_CONNECT:8, Id:8, 8:8, ClientKey:8/binary, _/binary>> ->
	    [{connect, ClientKey}, {response, accept, "OK"}];
	<<?TCP_HEAD_DISCONNECT:8, Id:8, _/binary>> ->
	    [{client,disconnect},close];
	<<?TCP_HEAD_FOLDER:8, Id:8, _:8, FolderID:32, _/binary>> ->
	    [{client, {folder, FolderID}}];
	<<?TCP_HEAD_FILE_EDIT:8, Id:8, _:8, FileID:32, _/binary>> ->
	    [{client, {open, FileID}}];
	<<?TCP_HEAD_FILE_SUBSCRIBE:8, Id:8, _:8, FileID:32, _/binary>> ->
	    [{client, {subscribe, FileID}}];
	<<?TCP_HEAD_FILE_CONTENT:8, Id:8, _/binary>> ->
	    [{client, content}];
	<<?TCP_HEAD_LINE_UPDATE:8, Id:8, Length:8, LineNum:32, Line:Length/binary, _/binary>> ->
	    [{client, {update, LineNum, Line}}];
	<<?TCP_HEAD_LINE_NEW:8, Id:8, Length:8, LineNum:32, Line:Length/binary, _/binary>> ->
	    [{client, {new_line, LineNum, Line}}];
	<<?TCP_HEAD_LINE_REMOVE:8, Id:8, _:8, LineNum:32, _/binary>> ->
	    [{client, {remove_line, LineNum}}]; 
	_ ->
	    ?log_wierd(["Unknown tcp message received"]),
	    [{response, error, "ERROR"}, close]
    end.

handle_action(Socket, Client, []) ->
    {Socket, Client};
handle_action(Socket, Client, [Action|Actions]) ->
    ?log_heavydebug(["Socket\r\n",Socket,"\r\nAction, ", Action]),
    case Action of
	{connect, ClientKey} ->
	    KeyRing = crypt:set_client_key(ClientKey, Socket#socket.keyring),
	    handle_action(Socket#socket{keyring=KeyRing}, Client, Actions);
	{response, Type, Response} ->
	    response(Socket, Type, Response),
	    handle_action(Socket, Client, Actions);
	{client, Message} ->
	    {Socket1, Client1} = client_message(Socket, Client, Message),
	    handle_action(Socket1, Client1, Actions);
	close ->
	    gen_tcp:close(Socket#socket.socket),
	    handle_action(Socket, Client, Actions)
    end.


client_message(Socket, Client, Message) ->
    client_handler:socket_message(Client,Message),
    {Socket, Client}.
