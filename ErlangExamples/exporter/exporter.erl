-module(exporter)
-export([init/0,push/1, pop/1])

init() ->
    

stack(S) ->
    receive
	{pop, From} ->
	    if length(S) = 0 ->
		    From ! -1;
	       true ->
		    From ! hd(S)
	    end,
	    stack(tl(S));
	{push, Val, From} ->
	    stack([V | S])
    end.
