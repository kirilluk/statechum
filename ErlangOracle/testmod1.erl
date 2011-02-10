-module(testmod1).
-export([testfun/1]).

testfun([]) ->
    ok;
testfun([I | IP]) ->
%%    io:format("1: ~p~n", [I]),
    if (I > 42) ->
	    testmod2:testfun2(I);
       (I == 37) ->
	    testmod2:testfun2(I + 19);
       true ->
	    testfun(IP)
    end.
