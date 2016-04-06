-module(locker).

-behaviour(gen_server).
-export([init/1, handle_call/2, handle_call/3, handle_cast/2, terminate/2]).


init(_) ->
    {ok, {unlocked, -1}}.

handle_call(lock, {unlocked, S}) ->
    {reply, {ok, locked}, {locked, S}};
handle_call(lock, {locked, _S}) ->
    erlang:error("Locked lock!");
handle_call(unlock, {unlocked, _S}) ->
    erlang:error("Unlocked unlock!");
handle_call(unlock, {locked, S}) ->
    {reply, {ok, unlocked}, {unlocked, S}};
handle_call(read, {State, S}) ->
    {reply, S, {State, S}};
handle_call({write, Val}, {locked, _S}) ->
    {reply, {ok, Val}, {locked, Val}};
handle_call({write, _Val}, {unlocked, _S}) ->
    erlang:error("Attempting unlocked write!").

handle_call(Msg, _From, State) ->
    handle_call(Msg, State).

handle_cast(stop, _State) ->
    {stop, normal, stopped};
handle_cast(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

