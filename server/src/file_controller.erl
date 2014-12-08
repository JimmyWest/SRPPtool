-module(file_controller).

-export([start/2, stop/1]).
-export([subscribe/2, unsubscribe/2, add_line/3, update_line/3, remove_line/2, get_line/2, save/1, close/1]).

-include("config.hrl").

-record(state, {file, key, subscribers=[]}).

start(Path,Filename) ->
    spawn(fun() -> init(Path,Filename) end).

stop(Pid) ->
    send_msg(Pid, stop).

init(Path, Filename) ->
    ?log_start(["Controller for: ",Path,Filename]),
    File = file_data:open(Path,Filename),
    ?log_heavydebug(["File path: (",Path,"\r\nFile:",File,")"]),
    Key = crypt:create_key(Path++Filename,8),
    State = #state{file=File, key=Key},
    recv_loop(State).

%% ### External API

subscribe(Pid, Client) ->
    send_msg(Pid, {subscribe, Client}).

unsubscribe(Pid, Client) ->
    send_msg(Pid, {unsubscribe, Client}).

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
    common:safe_send_sync(Pid, Msg).

recv_loop(State) ->
    {Msg,Com} = common:receive_msg(unlimited),
    case Msg of
	{subscribe, Client} ->
	    State1 = add_subscriber(State, Client),
	    common:reply(Com, self()),
	    recv_loop(State1);
	{unsubscribe, Client} ->
	    State1 = remove_subscriber(State, Client),
	    common:reply(Com, ok),
	    recv_loop(State1);
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

add_subscriber(State = #state{subscribers=Subscribers}, Client) ->
    %% send file copy to new subcriber.
    State#state{subscribers=[Client|Subscribers]}.

remove_subscriber(State = #state{subscribers=Subscribers}, Client) ->
    Subscribers1 = lists:delete(Client, Subscribers),
    State#state{subscribers=Subscribers1}.
