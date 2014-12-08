-module(srpp_server).

-export([start/0]).

-include("config.hrl").

start() ->
    case loadArgs([{wd,text},{server_key,text},{port,int}], []) of
	[WorkingDirectory, ServerKey, Port] ->
	    ?log_start(["SRPP server started!"]),
	    ?log_info(["Reading files from ",WorkingDirectory]),
	    ?log_info(["Listening on port ",Port]),
	    ?log_info(["ServerKey phrase is (",ServerKey,")"]),
	    directory_handler:start(WorkingDirectory),
	    port_handler:start(Port);
	{error, Reason} ->
	    ?log_error(["Error on start, due to ",Reason]),
	    print_help(Reason)
    end.


loadArgs({error, Reason}, _) -> {error, Reason};
loadArgs([], DataList) -> lists:reverse(DataList);
loadArgs([Arg|Args], DataList) ->
    ?log_load(["Argument ",Arg]),
    case loadArg(Arg) of
	{ok, Data} ->
	    loadArgs(Args, [Data|DataList]);
	Err = {error,_} ->
	    loadArgs(Err,[]);
	Err ->
	    loadArgs({error, Err}, [])
    end.

loadArg({Arg, int}) ->
    case init:get_argument(Arg) of
	{ok, [[Data]]} ->
	    case string:to_integer(Data) of
		Err = {error,_} -> Err;
		{Int, []} -> {ok, Int}
	    end;
	Err -> Err
    end;
loadArg({Arg, text}) ->
    case init:get_argument(Arg) of
	{ok, [[Data]]} ->
	    {ok, Data};
	Err ->
	    Err
    end.

print_help(Reason) ->
    io:format("Help text ~p~n",[Reason]).
