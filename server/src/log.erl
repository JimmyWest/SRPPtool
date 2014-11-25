-module(log).

-export([start/1, info/1, debug/1, err/1]).

-include("config.hrl").

start(_) ->
    Pid = spawn(fun() -> init() end),
    common:safe_register(logger, Pid),
    Pid.


init() ->
    recv_loop().

recv_loop() ->
    receive
	{log, Type, Msg} ->
	    log_data(Type, Msg),
	    recv_loop();
	stop ->
	    recv_loop_end()
    end.

recv_loop_end() ->
    receive
	{log, Type, Msg} ->
	    log_data(Type, Msg),
	    recv_loop_end()
    after 0 -> ok
    end.

log_data(Type, Msg) when is_list(Msg) ->
    print_info(),
    case Type of
	info ->
	    print_ln(["I: "|Msg]);
	error ->
	    print_ln(["E: "|Msg]);
	debug ->
	    print_ln(["DEBUG: "|Msg]);
	_ ->
	    print_ln([Type,": "|Msg])
    end;
log_data(Type, Msg) ->
    log_data(Type, [Msg]).

print_info() ->
    print_date().

print_date() ->
    {Y,M,D} = date(),
    {H,Mi,S} = time(),
    print(["[",Y,"-",z(M),"-",z(D),"](",z(H),":",z(Mi),":",z(S),")"]).

z(V) when V < 10 ->
    "0" ++ integer_to_list(V);
z(V) ->
    V.


print_ln(Data) ->
    print(Data),
    io:format("~n").

print([]) -> ok;
print([H|T]) when is_list(H) ->
    io:format("~s",[H]),
    print(T);
print([H|T]) ->
    io:format("~p",[H]),
    print(T).

info(Info) ->
    log(info, Info).

debug(Info) ->
    case ?debug of
	true ->
	    log(debug, Info);
	_ ->
	    ok
    end.

err(Info) ->
    log(error, Info).

log(Type, Msg) ->
   common:send_singleton(logger, common:mfa(log,start,[]), {log, Type, Msg}).
