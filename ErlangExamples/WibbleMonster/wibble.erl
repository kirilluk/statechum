-module(wibble).
-export([init/1,handle_call/3,handle_call/2,handle_cast/3,handle_cast/2,handle_info/3,handle_info/2,code_change/3,terminate/2]).

-behaviour(gen_server).

init(_Args) ->
    {ok, xyz1}.

handle_call(Event, State) ->
    handle_call(Event, xyz, State).

handle_call(xyz, _From, xyz1) ->
    {reply, wibbling, xyz2};
handle_call(xyz, _From, xyz2) ->
    {noreply, xyz1};
handle_call(_, _From, _) ->
    erlang:exit("NOT XYZ!!!!!").

handle_cast(Event, State) ->
    handle_cast(Event, xyz, State).

handle_cast(xyz, _From, State) ->
    {noreply, State};
handle_cast(_, _From, State) ->
    {stop, "Errr, not wibbling much!", State}.

handle_info(Event, State) ->
    handle_info(Event, xyz, State).

handle_info(_, _From, xyz2) ->
    {noreply, xyz1};
handle_info(xyz, _From, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    normal.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
