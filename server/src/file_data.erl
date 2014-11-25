-module(file_data).

-export([open/2, save/1, close/1, add_line/3, update_line/3, remove_line/2, get_line/2]).
-export([crlf/2]).

-record(file, {path, name, io, data}).

-include("config.hrl").

open(Path, Filename) ->
    {ok, IoDevice} = file:open(Path++Filename,[read,write]),
    read_file(#file{path=Path, name=Filename, io=IoDevice}).

read_file(File) ->
    Data = new(),
    read_file(File#file{data=Data},1).

read_file(File = #file{io=IoDevice},LN) ->
    case file:read_line(IoDevice) of
	eof ->
	    File;
	{ok, Line} when is_list(Line) ->
	    File1 = file_data:add_line(Line,LN,File),
	    read_file(File1,LN+1)
    end.

save(#file{io=IoDevice, data=Data}) ->
    FileData = pack_data(Data),
    file:position(IoDevice,{bof,0}),
    file:write(IoDevice, FileData).

pack_data(Data) ->
    Data2 = lists:flatten(Data),
    Bin = list_to_binary(crlf(Data2, [])),
    ?log_debug(["Packed file, size=",size(Bin),", data=",Bin]),
    Bin.

crlf([],L) -> lists:reverse(L); 
crlf([13,10|T], L) ->
    crlf(T,"\n\r" ++ L);
crlf([13|T], L) ->
    crlf(T,"\n\r" ++ L);
crlf([10|T], L) ->
    crlf(T,"\n\r" ++ L);
crlf([H|T], L) ->
    crlf(T,[H|L]).

close(#file{io=IoDevice}) ->
    file:close(IoDevice).

new() ->
    [].

%% Add error handling, so it not crashes when wrong.

add_line(Line,N,File = #file{data=Data}) ->
    {D1,D2} = lists:split(N-1,Data),
    File#file{data=D1 ++ [Line|D2]}.

update_line(Line,N,File = #file{data=Data}) ->
    {D1,[_|D2]} = lists:split(N-1, Data),
    File#file{data=D1 ++ [Line|D2]}.

remove_line(N, File = #file{data=Data}) ->
    {D1,[_|D2]} = lists:split(N-1,Data),
    File#file{data=D1++D2}.

get_line(N, #file{data=Data}) ->
    {_,[Line|_]} = lists:split(N-1,Data),
    Line.
