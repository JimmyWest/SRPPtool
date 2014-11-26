-module(log).

-export([test/0, start/1, stop/0, log/3]).

-include("config.hrl").

-record(conf, {modules=[], types=[], structure=[]}).

start(_) ->
    spawn(fun() -> init() end).

stop() ->
    logger ! stop.

%% Only one API function

log(Type, Info, Msg) ->
    common:send_singleton(logger, common:mfa(log,start,[]), {log, Type, Info, Msg}).

init() ->
    recv_loop(default_conf()).

default_conf() ->
    #conf{modules=?DEFAULT_MODULES, types=?DEFAULT_TYPES, structure=?LOG_STRUCTURE}.

recv_loop(Conf) ->
    receive
	{log, Type, Info, Msg}  ->
	    log_data(Conf, Type, Info, Msg),
	    recv_loop(Conf);
	stop ->
	    recv_loop_end(Conf)
    end.

recv_loop_end(Conf) ->
    receive
	{log, Type, Info, Msg}  ->
	    log_data(Conf, Type, Info, Msg),
	    recv_loop_end(Conf)
    after 0 -> ok
    end.

log_data(Conf = #conf{structure=Structure}, Type, Info, Msg) ->
    case isvalid(Conf, Type, Info) of
	true ->
	    Line = build_log(Structure, Type, Info, Msg),
	    Format = build_format(Line,[]),
	    io:format(Format,Line);
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

isvalid_info(#conf{modules=Modules}, {Module,_,_}) ->
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
build_log([module|Struct], Type, Info = {Module,_,_}, Msg, Line) ->
    build_log(Struct, Type, Info, Msg, [Module|Line]);
build_log([line|Struct], Type, Info = {_,_,N}, Msg, Line) ->
    build_log(Struct, Type, Info, Msg, [N|Line]);
build_log([type|Struct], Type = {_,_,T}, Info, Msg, Line) ->
    build_log(Struct, Type, Info, Msg, [T|Line]);
build_log([self|Struct], Type, Info, Msg, Line) ->
    build_log(Struct, Type, Info, Msg, [self()|Line]);
build_log([pid|Struct], Type, Info = {_,Pid,_}, Msg, Line) ->
    build_log(Struct, Type, Info, Msg, [Pid|Line]);
build_log([msg|Struct], Type, Info, Msg, Line) ->
    build_log(Struct, Type, Info, Msg, Msg++Line);
build_log([boc|Struct], Type = {_,Color,_}, Info, Msg, Line) ->
    build_log(Struct, Type, Info, Msg, [Color|Line]);
build_log([eoc|Struct], Type, Info, Msg, Line) ->
    build_log(Struct, Type, Info, Msg, [?END_COLOR|Line]);
build_log([H|Struct], Type, Info, Msg, Line) ->
    build_log(Struct, Type, Info, Msg, [H|Line]).

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

build_format([], Line) ->
    Line++"~n";
build_format([H|T], Line) when is_list(H) ->
    build_format(T, Line++"~s"); % String
build_format([_|T], Line) ->
    build_format(T,Line++"~p").

%% Test logger

test() ->
    Line = ["This is, ",auto,", ",23," end of line!"],
    ?log_start(Line),
    ?log_info(Line),
    ?log_load(Line),
    ?log_error(Line),
    ?log_debug(Line),
    ?log_heavydebug(Line),
    ?log_wierd(Line).
