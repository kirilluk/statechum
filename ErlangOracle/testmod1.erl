-module(testmod1).
-export([testfun/1]).

testfun([]) ->
    ok;
testfun([I | IP]) ->
%%    io:format("1: ~p~n", [I]),
    if (I > 32) ->
	    testmod2:testfun2(I);
       (I == 37) ->
	    testmod2:testfun2(I + 19);
	(I < 16) ->
	    erlang:exit("Not allowed...");
       true ->
	    testfun(IP)
    end.
