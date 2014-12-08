-module(directory_handler).

-export([start/0, start/1, stop/0, subscribe/1]).

-include("config.hrl").

-record(file, {id,dir,name,info,controller}).

-record(dir, {id,dir,name,structure,info}).

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

subscribe(Id) ->
    common:send_sync(directory_handler, {subscribe, Id}).

init(WorkingDirectory) ->
    case file:read_file_info(WorkingDirectory) of
	{error, Reason} ->
	    ?log_error(["Directory handler could not be started, due to: ",Reason]);
	{ok, #file_info{type=directory}} ->
	    ?log_start(["Directory handler started!"]),
	    ?log_info(["Loading directory: ",WorkingDirectory]),
	    init_fetch(WorkingDirectory)
    end.

init_fetch(WorkingDirectory) ->
    Files = new(),
    {Structure,_} = fetch_files(Files, WorkingDirectory,1),
    case ?debug of
	true ->
	    ?log_heavydebug(["Structure:\r\n",Structure]),
	    AllFiles = all_files(Files,ets:first(Files),[]),
	    ?log_heavydebug(["Files:\r\n"]++AllFiles);
	_ -> ok
    end,
    ?log_info(["Files and directories lists loaded."]),
    recv_loop(Files, Structure).

all_files(Files, Key, List) ->
    case Key of
	'$end_of_table' ->
	    List;
	_ ->
	    Elem = ets:lookup(Files,Key),
	    all_files(Files, ets:next(Files,Key), [{Elem},"\r\n"|List])
    end.
	    

fetch_files(Files, Dir,Id) ->
    Structure = structure:new(Dir),
    {ok, Content} = file:list_dir(Dir),
    parse_content(Files, Id, Structure, Dir++"/",Content).

parse_content(_, Id, Structure, _, []) ->
    {Structure, Id};
parse_content(Files, Id, Structure, Dir, [Entity|Content]) ->
    ?log_load(["File ",Dir++Entity," ..."]),
    case file:read_file_info(Dir++Entity) of
	{error, Reason} ->
	    ?log_error(["Could not read file info of ",Dir,Entity,", due to: ",Reason]),
	    parse_content(Files, Id, Structure, Dir, Content);
	{ok, FileInfo = #file_info{type=regular}} ->
	    add_file(Files, Id, Dir, Entity, FileInfo),
	    Structure1 = structure:add_file(Structure, Id, Entity),
	    parse_content(Files, Id+1, Structure1, Dir, Content);
	{ok, FileInfo = #file_info{type=directory}} ->
	    {DirStructure, Id1} = fetch_files(Files, Dir++Entity,Id+1),
	    add_dir(Files, Id, Dir, Entity, DirStructure, FileInfo),
	    Structure1 = structure:add_directory(Structure, Entity, DirStructure),
	    parse_content(Files, Id1, Structure1, Dir, Content);
	_ ->
	    ?log_debug(["Wierd entity in content: ",Dir,Entity]),
	    parse_content(Files, Id, Structure, Dir, Content)
    end.


%% File data structure functions

new() ->
    ets:new(files, [ordered_set]).

delete(Files) ->
    ets:delete(Files).

add_file(Files, Id, Dir, Name, Info) ->
    File = #file{id=Id, dir=Dir, name=Name, info=Info},
    ets:insert(Files, {Id, File}).

add_dir(Files, Id, Dir, Name, Structure, Info) ->
    Entry = #dir{id=Id, dir=Dir, name=Name, structure=Structure, info=Info},
    ets:insert(Files, {Id, Entry}).

update_file(Files, Id, File) ->
    ets:insert(Files, {Id, File}).

lookup(Files, Id) ->
    case ets:lookup(Files, Id) of
	[] -> undefined;
	[File] -> File;
	List -> hd(List)
    end.

set_controller(Files, File = #file{id=Id}, Controller) ->
    File1 = File#file{controller=Controller},
    update_file(Files, Id, File1).


get_controller(Files, Id) ->
    case lookup(Files, Id) of
	{Id, File = #file{dir=Dir, name=Name, controller=undefined}} ->
	    C = file_controller:start(Dir,Name),
	    set_controller(Files, File, C),
	    C;
	{Id, #file{controller=C}} -> C;
	File ->
	    ?log_error(["Wierd file in Files. File: ", File]),
	    undefined
    end.

recv_loop(Files, Structure) ->
    {Msg, Com} = common:receive_msg(unlimited),
    case Msg of
	{subscribe, Id} ->
	    Client = common:get_pid_from_com(Com),
	    Controller = subscribe_to_file(Files, Client, Id),
	    common:reply(Com, Controller),
	    recv_loop(Files, Structure);
	stop ->
	    ?log_info(["Directory handler ",self()," are stopped."]),
	    delete(Files),
	    common:reply(Com, stopped);
	Msg ->
	    ?log_debug(["Got wierd message: ",Msg]),
	    recv_loop(Files, Structure)
    end.

subscribe_to_file(Files, Client, Id) ->
    Controller = get_controller(Files, Id),
    file_controller:subscribe(Controller, Client).
