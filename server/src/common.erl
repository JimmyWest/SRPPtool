-module(common).

-export([safe_register/2, mfa/3, send_singleton/3, send_sync/2, send/2, response/1, receive_msg/1, reply/2]).

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
    Pid = case whereis(Name) of
	      undefined ->
		  P = M:F(A),
		  safe_register(Name, P),
		  P;
	      P ->
		  P
	  end,
    Pid ! Msg.

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
	    {timeout, undefined}
    end.

reply(undefined, _) -> ok;
reply({Pid,Ref}, Msg) ->
    Pid ! {response, Ref, Msg}.
