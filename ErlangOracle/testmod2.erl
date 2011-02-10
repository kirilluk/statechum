-module(testmod2).
-export([testfun2/1]).

testfun2([]) ->
    ok;
testfun2(I) ->
%%    io:format("--2: ~p~n", [I]),
    if (I < 42) ->
	    erlang:error("WIBBLE!");
       true ->
	    testmod1:testfun([I-5])
    end.
