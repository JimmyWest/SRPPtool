-module(log).

-export([info/1, debug/1, err/1]).

log_data(Type, Msg) when is_list(Msg) ->
    print_info(),
    case Type of
	info ->
	    print_ln(["I: "|Msg]);
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
    log_data(info, Info).

debug(Info) ->
    log_data(debug, Info).

err(Info) ->
    log_data(error, Info).
