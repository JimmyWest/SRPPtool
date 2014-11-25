-module(log).

-export([start/1, stop/0, info/2, info/1, debug/2, debug/1, err/2, err/1]).

-include("config.hrl").

start(_) ->
    Pid = spawn(fun() -> init() end),
    Pid.

stop() ->
    logger ! stop.

info(Info, Msg) ->
    log(info, Info, Msg).
info(Msg) ->
    log(info, Msg).

debug(Info, Msg) ->
    log(debug, Info, Msg).
debug(Msg) ->
    case ?debug of
	true ->
	    log(debug, Msg);
	_ ->
	    ok
    end.
err(Info,Msg) ->
    log(error,Info,Msg).
err(Msg) ->
    log(error, Msg).

log(Type, Info, Msg) ->
    common:send_singleton(looger, common:mfa(log,start,[]), {log, Type, Info, Msg}).

log(Type, Msg) ->
    common:send_singleton(logger, common:mfa(log,start,[]), {log, Type, Msg}).

init() ->
    recv_loop().

recv_loop() ->
    receive
	{log, Type, Msg} ->
	    log_data(Type, Msg),
	    recv_loop();
	{log, Type, Info, Msg} ->
	    log_data(Type, [Info,", "|Msg]),
	    recv_loop();
	stop ->
	    recv_loop_end()
    end.

recv_loop_end() ->
    receive
	{log, Type, Msg} ->
	    log_data(Type, Msg),
	    recv_loop_end();
	{log, Type, Info, Msg} ->
	    log_data(Type, [Info|Msg]),
	    recv_loop_end()
    after 0 -> ok
    end.

log_data(Type, Msg) when is_list(Msg) ->
    print_info(),
    case Type of
	info ->
	    print_ln([?INFO_COLOR, "I: "|Msg],true);
	error ->
	    print_ln([?ERROR_COLOR, "err: "|Msg], true);
	debug ->
	    print_ln([?DEBUG_COLOR, "DEBUG: "|Msg], true);
	_ ->
	    print_ln([Type,": "|Msg],false)
    end;
log_data(Type, Msg) ->
    log_data(Type, [Msg]).

print_info() ->
    print_date().

print_date() ->
    {Y,M,D} = date(),
    {H,Mi,S} = time(),
    print(["[",Y,?DATE_SEP,z(M),?DATE_SEP,z(D),?DATE_TIME_SEP,z(H),?TIME_SEP,z(Mi),?TIME_SEP,z(S),"]"]).

z(V) when V < 10 ->
    "0" ++ integer_to_list(V);
z(V) ->
    V.


print_ln(Data, Color) ->
    print(Data),
    case Color of
	true->
	    io:format("~s~n",[?END_COLOR]);
	_ ->
	    io:format("~n")
    end.

print([]) -> ok;
print([H|T]) when is_list(H) ->
    io:format("~s",[H]),
    print(T);
print([H|T]) ->
    io:format("~p",[H]),
    print(T).
