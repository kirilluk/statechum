-module(exporter).
-export([init/0, push/1, pop/0, stack/1, stop/0]).

init() ->
    Pid = spawn(exporter, stack, [[]]),
    register(thestack, Pid),
    true.

stop() ->
    thestack ! die.

stack(S) ->
    receive
	{pop, From} ->
	    if (length(S) == 0) ->
		    From ! -1,
		    stack(S);
	       true ->
		    From ! hd(S),
		    stack(tl(S))
	    end;
	{push, Val, From} ->
	    From ! ok,
	    stack([Val | S]);
	die ->
	   S
    end.

pop() ->
    thestack ! {pop, self()},
    receive
	Val ->
	    Val
    end.

push(Val) ->
    thestack ! {push, Val, self()},
    receive
	ok ->
	    ok;
	_ ->
	    error
    end.
