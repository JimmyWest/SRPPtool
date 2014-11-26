-module(directory_handler).

-export([start/0, start/1, stop/0, subscribe/1]).

-include("config.hrl").

-record(file, {id,dir,name,info,controller}).

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

stop() ->
    common:safe_send_sync(directory_handler, stop).

subscribe(ID) ->
    common:send_sync(directory_handler, {subscribe, ID}).

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
    case ?debug of
	true ->
	    ?log_debug(["Structure:\r\n",Structure]),
	    AllFiles = all_files(Files,ets:first(Files),[]),
	    ?log_debug(["Files:\r\n"]++AllFiles);
	_ -> ok
    end,
    ?log_info(["Files and directories lists loaded."]),
    recv_loop(Files, Structure).

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

delete(Files) ->
    ets:delete(Files).

add_file(Files, Dir, Name, Info) ->
    ID = make_ref(),
    File = #file{id=ID, dir=Dir, name=Name, info=Info},
    ets:insert(Files, {ID, File}),
    ID.

update_file(Files, ID, File) ->
    ets:insert(Files, {ID, File}).

lookup(Files, ID) ->
    case ets:lookup(Files, ID) of
	[] -> undefined;
	[File] -> File;
	List -> hd(List)
    end.

set_controller(Files, File = #file{id=ID}, Controller) ->
    File1 = File#file{controller=Controller},
    update_file(Files, ID, File1).


get_controller(Files, ID) ->
    case lookup(Files, ID) of
	File = #file{dir=Dir, name=Name, controller=undefined} ->
	    C = file_controller:start(Dir,Name),
	    set_controller(Files, File, C),
	    C;
	#file{controller=C} -> C;
	File ->
	    ?log_error(["Wierd file in Files. File: ", File]),
	    undefined
    end.

recv_loop(Files, Structure) ->
    {Msg, Com} = common:receive_msg(unlimited),
    case Msg of
	{subscribe, ID} ->
	    Client = common:get_pid_from_com(Com),
	    Controller = subscribe_to_file(Files, Client, ID),
	    common:reply(Com, Controller),
	    recv_loop(Files, Structure);
	stop ->
	    delete(Files),
	    common:reply(Com, stopped);
	Msg ->
	    ?log_debug(["Got wierd message: ",Msg]),
	    recv_loop(Files, Structure)
    end.

subscribe_to_file(Files, Client, ID) ->
    Controller = get_controller(Files, ID),
    file_controller:subscribe(Controller, Client).
