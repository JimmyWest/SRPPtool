-module(directory_handler).

-export([start/0, start/1]).

-include("config.hrl").

-record(file_info, {
	  size,
	  type,
	  access,
	  atime,
	  mtime,
	  ctime,
	  mode,
	  links,
	  major_device,
	  minor_device,
	  inode,
	  uid,
	  gid}
).

start() ->
    {ok, WorkingDirectory} = file:get_cwd(),
    start(WorkingDirectory).

start(WorkingDirectory) ->
    Pid = spawn(fun() -> init(WorkingDirectory) end),
    common:safe_register(directory_handler, Pid).

init(WorkingDirectory) ->
    case file:set_cwd(WorkingDirectory) of
	{error, Reason} ->
	    ?log_error(["Directory handler could not be started, due to: ",Reason]);
	ok ->
	    ?log_info(["Directory handler started!"]),
	    init()
    end.

init() ->
    Files = new(),
    Structure = fetch_files(Files, "."),
    ?log_debug(["Structure:\r\n",Structure]),
    AllFiles = all_files(Files,ets:first(Files),[]),
    ?log_debug(["Files:\r\n"]++AllFiles).

all_files(Files, Key, List) ->
    ?log_debug(["+"]),
    case Key of
	'$end_of_table' ->
	    List;
	_ ->
	    Elem = ets:lookup(Files,Key),
	    all_files(Files, ets:next(Files,Key), [{Elem},"\r\n"|List])
    end.
	    

fetch_files(Files, Dir) ->
    Structure = structure:new(Dir),
    {ok, Content} = file:list_dir(Dir),
    parse_content(Files, Structure,Dir++"/",Content).

parse_content(_, Structure, _, []) ->
    Structure;
parse_content(Files, Structure, Dir, [Entity|Content]) ->
    case file:read_file_info(Dir++Entity) of
	{error, Reason} ->
	    ?log_error(["Could not read file info of ",Dir++Entity,", due to: ",Reason]),
	    parse_content(Files, Structure, Dir, Content);
	{ok, FileInfo = #file_info{type=regular}} ->
	    ID = add_file(Files, Dir, Entity, FileInfo),
	    Structure1 = structure:add_file(Structure, ID, Entity),
	    parse_content(Files, Structure1, Dir, Content);
	{ok, #file_info{type=directory}} ->
	    DirStructure = fetch_files(Files, Dir++Entity),
	    Structure1 = structure:add_directory(Structure, Entity, DirStructure),
	    parse_content(Files, Structure1, Dir, Content);
	_ ->
	    ?log_debug(["Wierd entity in content: ",Dir,Entity]),
	    parse_content(Files, Structure, Dir, Content)
    end.


%% File data structure functions

new() ->
    ets:new(files, [ordered_set]).

add_file(Files, Dir, Name, Info) ->
    ID = make_ref(),
    ets:insert(Files, {ID, Dir, Name, Info}),
    ID.
