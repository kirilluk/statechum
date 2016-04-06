-module(event).
-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2]).

init(_Args) ->
    {ok, []}.
handle_event(flip, a) ->
    {ok, b};
handle_event(flip, b) ->
    {ok, a};
handle_event(_Msg, State) ->
    {ok, State}.
terminate(_Args, _State) ->
    ok.
