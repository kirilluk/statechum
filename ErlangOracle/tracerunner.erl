%%% -------------------------------------------------------------------
%%% Author  : kirr
%%% Description : Runs traces on Erlang modules and reports results.
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

start([Node]) ->
	{ ok, _Pid } = gen_server:start_link({local,tracecheckServer},tracerunner,[],[]),
	NodeAsAtom = case is_atom(Node) of
		true ->Node;
		false ->list_to_atom(Node)
    end,
 	verifyNodeUp(NodeAsAtom).

%% Waits for the Java process to terminate and then shuts down the server.
verifyNodeUp(Node) ->
	timer:sleep(500),
	case (net_adm:ping(Node)) of
		pong -> verifyNodeUp(Node);
		pang -> erlang:halt() %% terminate node
	end.
	
makeCall(Where,Arg) ->gen_server:call({tracecheckServer,Where},Arg).

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

