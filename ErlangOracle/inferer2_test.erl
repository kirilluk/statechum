-module(inferer2_test).
-export([test_fun/1, choccy/1]).

test_fun(Input) ->
    try test_internal(1, Input) of
	Result ->
	    Result
    catch error:E ->
	    erlang:exit(E)
    end.

test_internal(State, []) ->
    State;
test_internal(State, [I | Input]) ->
    test_internal(test_state(State, I), Input).

test_state(1, a) ->
    2;
test_state(1, b) ->
    3;
test_state(2, a) ->
    4;
test_state(3, a) ->
    4;
test_state(4, b) ->
    3;
test_state(4, a) ->
    5;
test_state(5, a) ->
    6.
%%test_state(State, []) ->
%%    State.

choccy([]) ->
    ok;
choccy(IP) ->
    choccy_state(0, IP).

choccy_state(_, []) ->
    ok;
choccy_state(0, [coin | IP]) ->
    choccy_state(1, IP);
choccy_state(1, [choc | IP]) ->
    choccy_state(0, IP);
choccy_state(N, [coin | IP]) when (N > 0) ->
    choccy_state(N+1, IP);
choccy_state(N, [choc | IP]) when (N > 0) ->
    choccy_state(N-1, IP);
choccy_state(_, _) ->
    erlang:exit("Not allowed...").
