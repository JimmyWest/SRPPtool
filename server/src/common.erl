-module(common).

-export([safe_register/1]).

safe_register(Name, Pid) ->
    case whereis(Name) of
	undefined ->
	    ok;
	P -> unregister(Name),
	     P ! stop
    end,
    register(Name,Pid).
