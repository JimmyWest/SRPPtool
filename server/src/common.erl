-module(common).

-export([safe_register/2, mfa/3, send_singleton/3, safe_send_sync/2, safe_send/2, send_sync/2, send/2, response/2, response/1, response/0, receive_msg/1, reply/2]).

safe_register(Name, Pid) ->
    case erlang:whereis(Name) of
	undefined ->
	    ok;
	P -> erlang:unregister(Name),
	     P ! stop
    end,
    erlang:register(Name,Pid).

mfa(Module,Function,Arguments) ->
    {Module,Function,Arguments}.

send_singleton(Name, {M,F,A}, Msg) ->
    Pid = case erlang:whereis(Name) of
	      undefined ->
		  P = M:F(A),
		  safe_register(Name, P),
		  P;
	      P ->
		  P
	  end,
    Pid ! Msg,
    ok.

safe_send_sync(Dest, Msg) ->
    case isalive(Dest) of
	fasle ->
	    Ref = echo(stopped),
	    response(Ref);
	_ ->
	    send_sync(Dest, Msg)
    end.

safe_send(Dest, Msg) ->
    case isalive(Dest) of
	false ->
	    echo(stopped);
	_ ->
	    send(Dest, Msg)
    end.

isalive(Dest) when is_atom(Dest) ->
    case erlang:whereis(Dest) of
	undefined ->
	    false;
	_ -> true
    end;
isalive(Dest) when is_pid(Dest) ->
    case erlang:process_info(Dest) of
	undefined ->
	    false;
	_ -> true
    end;
isalive(_) ->
    false.

echo(Msg) ->
    Ref = make_ref(),
    self() ! {response, Ref, Msg},
    Ref.

send_sync(Destination, Msg) ->
    Ref = send(Destination, Msg),
    response(Ref).

send(Destination, Msg) ->
    Ref = make_ref(),    
    Destination ! {msg, self(), Ref, Msg},
    Ref.

response(Ref, Timeout) ->
    receive
	{response, Ref, Msg} ->
	    Msg
    after Timeout -> timeout
    end.

response(Ref) ->
    receive
	{response, Ref, Msg} ->
	    Msg
    end.

response() ->
    receive
	{response, _, Msg} -> Msg
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
	    {timeout, undefined}
    end.

reply(undefined, _) -> ok;
reply({Pid,Ref}, Msg) ->
    Pid ! {response, Ref, Msg}.
