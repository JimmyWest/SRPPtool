-module(common).

-export([safe_register/2, send_sync/2, send/2, response/1, receive_msg/1, reply/2]).

safe_register(Name, Pid) ->
    case erlang:whereis(Name) of
	undefined ->
	    ok;
	P -> erlang:unregister(Name),
	     P ! stop
    end,
    erlang:register(Name,Pid).

send_sync(Pid, Msg) ->
    Ref = send(Pid, Msg),
    response(Ref).

send(Pid, Msg) ->
    Ref = make_ref(),    
    Pid ! {msg, self(), Ref, Msg},
    Ref.

response(Ref) ->
    receive
	{response, Ref, Msg} ->
	    Msg
    end.

receive_msg(unlimited) ->
    receive
	{msg, Pid, Ref, Msg} ->
	    {Msg, {Pid,Ref}}
    end;

receive_msg(Timeout) ->
    receive
	{msg, Pid, Ref, Msg} ->
	    {Msg, {Pid,Ref}}
    after Timeout ->
	    {timeout,undefined}
    end.

reply({Pid,Ref}, Msg) ->
    Pid ! {response, Ref, Msg}.
