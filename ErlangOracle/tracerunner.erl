%%% -------------------------------------------------------------------
%%% Author  : kirr
%%% Description : Runs traces on Erlang modules and reports results.
%%% Copyright (c) 2011 The University of Sheffield
%%% 
%%% This file is part of StateChum
%%% 
%%% StateChum is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%% 
%%% StateChum is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%% 
%%% You should have received a copy of the GNU General Public License
%%% along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%% Created : 13 Apr 2011
%%% -------------------------------------------------------------------
-module(tracerunner).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Starts the server process and monitors the Java runtime, terminating Erlang node
%% when Java fails to respond.
-export([start/1]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================

start(Args)->startRunner(strings_to_atoms(Args)).

%% Production use
startRunner([Node,tracerunner]) ->
	{ ok, _Pid } = gen_server:start_link({local,tracecheckServer},tracerunner,[],[]),
	verifyJavaUp(Node);

%% For testing
startRunner([Node,noserver])->verifyJavaUp(Node);
startRunner([_Node,halt])->halt();
startRunner([_Node,error])->erlang:error("startup error").

strings_to_atoms([])->[];
strings_to_atoms([Head|Tail])->
    [case is_atom(Head) of
		true ->Head;
		false ->list_to_atom(Head)
    end | strings_to_atoms(Tail)].


%% Waits for the Java process to terminate and then shuts down the server.

verifyJavaUp(Node) ->
	timer:sleep(500),
	case (net_adm:ping(Node)) of
		pong -> verifyJavaUp(Node);
		pang -> erlang:halt() %% terminate node
	end.
	
%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({runTrace,Trace}, _From, State) ->
	io:format("~w~n", [Trace]),
    	Reply = {ok,aa},
    	{reply, Reply, State};

%% Used for testing - does not produce a response.
handle_call(timeout,_From, State) ->
	{noreply, State};

%% Used for testing - produces a specific response.
handle_call({echo,[Head | Tail]},_From, State) ->
	{reply, { Head, Tail }, State};

%% Compiles all supplied modules
handle_call({compile,[]}, _From, State) ->
	{reply, ok, State};
	
handle_call({compile,[M | OtherModules]}, From, State) ->
	case(cover:compile(M)) of
		{ok,_} -> handle_call({compile,OtherModules}, From, State);
		_ -> {reply, failed, State}
	end.
	
%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

