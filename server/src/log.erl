-module(log).

-export([test/0, start/1, stop/0, log/3, info/2, info/1, debug/2, debug/1, err/2, err/1]).

-include("config.hrl").

-record(conf, {modules=[], types=[], structure=[]}).

start(_) ->
    spawn(fun() -> init() end).

stop() ->
    logger ! stop.

%% API functions (Legacy)

info(Info, Msg) ->
    log(info, Info, Msg).
info(Msg) ->
    log(info, Msg).

debug(Info, Msg) ->
    case ?debug of
	true ->
	    log(debug, Info, Msg);
	_ -> ok
    end.
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

%% Only one API function

log(Type, Info, Msg) ->
    common:send_singleton(logger, common:mfa(log,start,[]), {log, Type, Info, Msg}).

log(Type, Msg) -> % Legacy
    common:send_singleton(logger, common:mfa(log,start,[]), {log, Type, {unknown,0}, Msg}).

init() ->
    recv_loop(default_conf()).

default_conf() ->
    #conf{modules=?DEFAULT_MODULES, types=?DEFAULT_TYPES, structure=?LOG_STRUCTURE}.

recv_loop(Conf) ->
    receive
	{log, Type, Msg} -> % legacy
	    log_data(Type, Msg),
	    recv_loop(Conf);
%	{log, debug, Info, Msg} ->
%	    log_data(debug, [Info,", "|Msg]),
%	    recv_loop(Conf);
%	{log, error, Info, Msg} ->
%	    log_data(error, [Info,", "|Msg]),
%	    recv_loop(Conf);
	{log, Type, Info, Msg}  ->
	    log_data(Conf, Type, Info, Msg),
	    recv_loop(Conf);
	stop ->
	    recv_loop_end()
    end.

recv_loop_end() ->
    receive
	{log, Type, Msg} ->
	    log_data(Type, Msg),
	    recv_loop_end();
	{log, debug, Info, Msg} ->
	    log_data(debug, [Info,", "|Msg]),
	    recv_loop_end();
	{log, Type, _, Msg} ->
	    log_data(Type, Msg),
	    recv_loop_end()
    after 0 -> ok
    end.

log_data(Conf = #conf{structure=Structure}, Type, Info, Msg) ->
    case isvalid(Conf, Type, Info) of
	true ->
	    Line = build_log(Structure, Type, Info, Msg),
	    print_nl(Line);
	_ -> ok
    end.

isvalid(Conf, Type, Info) ->
    case isvalid_type(Conf, Type) of
	true ->
	    case isvalid_info(Conf, Info) of
		true ->
		    true;
		_ ->
		    false
	    end;
	_ ->
	    false
    end.

isvalid_type(#conf{types=Types}, {Type,_,_}) ->
    case Types of
	all -> true;
	_ -> lists:member(Type,Types)
    end.

isvalid_info(#conf{modules=Modules}, {Module,_}) ->
    case Modules of
	all -> true;
	_ -> lists:member(Module,Modules)
    end.

build_log(Struct, Type, Info, Msg) ->
    build_log(lists:reverse(Struct), Type, Info, Msg, []).

build_log([], _, _, _, Line) ->
    Line;
build_log([date|Struct], Type, Info, Msg, Line) ->
    build_log(Struct, Type, Info, Msg, date_data()++Line);
build_log([time|Struct], Type, Info, Msg, Line) ->
    build_log(Struct, Type, Info, Msg, time_data()++Line);
build_log([module|Struct], Type, Info = {Module,_}, Msg, Line) ->
    build_log(Struct, Type, Info, Msg, [Module|Line]);
build_log([line|Struct], Type, Info = {_,N}, Msg, Line) ->
    build_log(Struct, Type, Info, Msg, [N|Line]);
build_log([type|Struct], Type = {_,_,T}, Info, Msg, Line) ->
    build_log(Struct, Type, Info, Msg, [T|Line]);
build_log([msg|Struct], Type, Info, Msg, Line) ->
    build_log(Struct, Type, Info, Msg, Msg++Line);
build_log([boc|Struct], Type = {_,Color,_}, Info, Msg, Line) ->
    build_log(Struct, Type, Info, Msg, [Color|Line]);
build_log([eoc|Struct], Type, Info, Msg, Line) ->
    build_log(Struct, Type, Info, Msg, [?END_COLOR|Line]);
build_log([H|Struct], Type, Info, Msg, Line) ->
    build_log(Struct, Type, Info, Msg, [H|Line]).

log_data(Type, Msg) when is_list(Msg) -> % Legacy
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

date_data() ->
    {Y,M,D} = date(),
    [Y,?DATE_SEP,z(M),?DATE_SEP,z(D)].

time_data() ->
    {H, M, S} = time(),
    [H,?TIME_SEP,M,?TIME_SEP,S].

z(V) when V < 10 ->
    "0" ++ integer_to_list(V);
z(V) ->
    V.

print_nl([]) ->
    io:format("~n");
print_nl([H|T]) when is_list(H) ->
    io:format("~s",[H]),
    print_nl(T);
print_nl([H|T]) ->
    io:format("~p",[H]),
    print_nl(T).

print_ln(Data, Color) -> % Legacy
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

%% Test logger

test() ->
    Line = ["String: This is, ",auto,", ",23,{"this",number}],
    ?log_start(Line),
    ?log_info(Line),
    ?log_load(Line),
    ?log_error(Line),
    ?log_debug(Line),
    ?log_heavydebug(Line),
    ?log_wierd(Line).
