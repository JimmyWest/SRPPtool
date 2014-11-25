-module(file_controller).

-export([start/2, stop/1]).
-export([add_line/3, update_line/3, remove_line/2, get_line/2, save/1, close/1]).

-record(state, {file, clienthandler}).

start(Path,Filename) ->
    spawn(fun() -> init(Path,Filename) end).

stop(Pid) ->
    send_msg(Pid, stop).

init(Path, Filename) ->
    File = file_data:open(Path,Filename),
    ClientHandler = ok,%client_handler:start(),
    State = #state{
	       file=File,
	       clienthandler=ClientHandler},
    recv_loop(State).

%% ### External API

add_line(Pid, N, Line) ->
    send_msg(Pid, {add_line, N, Line}).

update_line(Pid, N, Line) ->
    send_msg(Pid, {update_line, N, Line}).

remove_line(Pid, N) ->
    send_msg(Pid, {remove_line, N}).

get_line(Pid, N) ->
    send_msg(Pid, {get_line, N}).

save(Pid) ->
    send_msg(Pid, save).

close(Pid) ->
    send_msg(Pid, close).

%% ### Internal functions

send_msg(Pid, Msg) ->
    common:send_sync(Pid, Msg).

recv_loop(State) ->
    {Msg,Com} = common:receive_msg(unlimited),
    case Msg of
	{add_line, N, Line} ->
	    File = file_data:add_line(Line,N,State#state.file),
	    common:reply(Com, ok),
	    recv_loop(State#state{file=File});
	{update_line, N, Line} ->
	    File = file_data:update_line(Line,N,State#state.file),
	    common:reply(Com, ok),
	    recv_loop(State#state{file=File});
	{remove_line, N} ->
	    File = file_data:remove_line(N, State#state.file),
	    common:reply(Com, ok),
	    recv_loop(State#state{file=File});
	{get_line, N} ->
	    Line = file_data:get_line(N, State#state.file),
	    common:reply(Com, Line),
	    recv_loop(State);
	save ->
	    Res = file_data:save(State#state.file),
	    common:reply(Com, Res),
	    recv_loop(State);
	stop ->
	    common:reply(Com, stopped),
	    ok;
	close ->
	    Res = file_data:close(State#state.file),
	    common:reply(Com, Res),
	    ok
    end.
