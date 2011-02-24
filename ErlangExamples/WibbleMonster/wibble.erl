-module(wibble).
-export([init/1,handle_call/3,handle_call/2,handle_cast/3,handle_cast/2,handle_info/3,handle_info/2,code_change/3,terminate/2]).

-behaviour(gen_server).

init(hello) ->
    {ok, xyz1};
init([wibble | _List]) ->
    {ok, xyz2};
init(_arg) ->
    {ok, xyz1}.

handle_call(Event, State) ->
    handle_call(Event, xyz, State).

handle_call(xyz, _From, xyz1) ->
    {reply, wibbling, xyz2};
handle_call(xyz, _From, xyz2) ->
    {reply, wobbling, xyz3};
handle_call(xyz, _From, xyz3) ->
    {reply, here_kirill, xyz1};
handle_call([abc | _List], _From, xyz2) ->
    {reply, listing, xyz1};
handle_call(_, _From, _) ->
    erlang:exit("NOT XYZ!!!!!").

handle_cast(stop, State) ->
    {stop, "Terminating", State};
handle_cast(_Event, State) ->
    {stop, "Errr, not wibbling much!", State}.

%% Deliberately broken in state xyz2
handle_cast(xyz, _From, State) ->
    {noreply, State};
handle_cast(xyz3, _From, State) ->
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
