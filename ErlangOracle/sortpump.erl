-module(sortpump).
-include_lib("eunit/include/eunit.hrl").
-export([sort/1, sortPumpStart/1, generate/1]).

small_test() ->
    [1,2,3,4,5] = sort([5,4,3,2,1]),
    [1,2,3,4,5] = sort([4,5,3,1,2]).

medium_test() ->
    TestList = generate(1000),
    lists:sort(TestList) =:= sort(TestList).

large_test() ->
    TestList = generate(10000),
    lists:sort(TestList) =:= sort(TestList).

huge_test() ->
    TestList = generate(100000),
    lists:sort(TestList) =:= sort(TestList).

generate(0) ->
    [];
generate(N) ->
    [round(random:uniform() * N)] ++ generate(N-1).

createPumps(0, Tgt) ->
    Tgt;
createPumps(N, Tgt) ->
    Pid = createPumps(N-1, Tgt),
    spawn(sortpump, sortPumpStart, [Pid]).

transmitList([X], Tgt) ->
    Tgt ! {fin, X};
transmitList([X|XS], Tgt) ->
    Tgt ! {item, X},
    transmitList(XS, Tgt).

receiveList() ->
    receive
	{item, Item} ->
	    L = receiveList(),
	    [Item | L];
	{fin, Item} ->
	    [Item]
    end.

sort(L) ->
    First = createPumps(length(L), self()),
    transmitList(L, First),
    receiveList().

sortPumpStart(Next) ->
    receive
	{item, Val} ->
	    sortPump(Next, Val);
	{fin, Val} ->
	    Next ! {fin, Val}
    end.

sortPump(Next, Val) ->
    receive 
	{item, Item} ->
	    case Item > Val of
		true ->
		    Next ! {item, Val},
		    sortPump(Next, Item);
		false ->
		    Next ! {item, Item},
		    sortPump(Next, Val)
	    end;
	{fin, Item} ->
	    case Item > Val of
		true ->
		    Next ! {item, Val},
		    Next ! {fin, Item};
		false ->
		    Next ! {item, Item},
		    Next ! {fin, Val}
	    end
    end.
