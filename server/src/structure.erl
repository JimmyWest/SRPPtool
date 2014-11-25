-module(structure).

-export([new/1, add_file/3, add_directory/3]).

-record(structure, {id=structure,name,content=[]}).
-record(file, {id, name}).
-record(dir, {id=dir, name, structure}).

new(Name) ->
    #structure{name=Name}.

add_file(Structure, ID, Name) ->
    File = #file{id=ID, name=Name},
    add_content(Structure, File).

add_directory(Structure, Name, DirStructure) ->
    Dir = #dir{name=Name, structure=DirStructure},
    add_content(Structure, Dir).

add_content(Structure, Entity) ->
    OldContent = Structure#structure.content,
    UnSortedContent = [Entity|OldContent],
    Content = sort_content(UnSortedContent),
    Structure#structure{content=Content}.

sort_content(Content) ->
    lists:keysort(1, lists:keysort(3, Content)).
