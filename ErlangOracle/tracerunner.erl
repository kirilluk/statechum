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

-record(state, {processNum,compiledModules=sets:new()}).

%% ====================================================================
%% External functions
%% ====================================================================

start(Args)->startRunner(strings_to_atoms(Args)).

%% For testing
startRunner([Node,noserver])->verifyJavaUp(Node);
startRunner([_Node,halt])->halt();
startRunner([_Node,error])->erlang:error("startup error");

%% Production use
startRunner([Node,Arg]) ->
	{ ok, _Pid } = gen_server:start_link({local,tracecheckServer},tracerunner,[Arg],[]),
	verifyJavaUp(Node).



strings_to_atoms(List)->
lists:map(fun(F) -> 
	if 
		is_atom(F) -> F;
		true	-> list_to_atom(F)
	end end,List).


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
init([Arg]) ->
    {ok, #state{processNum=Arg}}.

%% Loads the supplied erl directly, can be used to substitute an arbitrary Erlang module with that of our own.
%% Invented to replace Typer modules, but since I had to replace the main module, this function is not used. 
compileAndLoad(What,Path) ->
	{ok,Bin,_}=compile:file(filename:join(Path,What),[verbose,debug_info,binary]),
	ModuleName = filename:basename(What,".erl"),
	code:purge(ModuleName),
	{module, _}=code:load_binary(ModuleName,"in_memory"++atom_to_list(What),Bin).


%% Compiles all supplied modules for Analyser
%% Modules which have already been compiled are recorded and not recompiled later.
compileModules([], State) ->
	{reply, ok, State};

compileModules([M | OtherModules], State) ->
	case sets:is_element(M,State#state.compiledModules) of
		true -> compileModules(OtherModules, State);
		false->
			case(cover:compile(M)) of
				{ok,_} -> compileModules(OtherModules, 
					State#state{compiledModules=sets:add_element(M,State#state.compiledModules)});
				FailureDetails -> {reply, {failed, FailureDetails}, State}
			end
	end.

%% Sometimes, files are known under different names but define the same module,
%% Dialyzer may lock up inside dialyzer_succ_typings:analyze_callgraph when 
%% analysing such files. The following function check for this and complains when 
%% file name does not match module name. Expects beams and throws an error if an
%% inconsistency is detected.
fileNameValid([])->ok;
fileNameValid([F|Others])->
	%% the next line is almost verbatim from typer.erl
	FileName = list_to_atom(filename:basename(F, ".beam")),
	{'module',ModName}=lists:keyfind('module',1,beam_lib:info(F)),
	case(FileName =:= ModName) of
		true -> fileNameValid(Others);
		false-> erlang:error("Invalid file name " ++ F ++ " for module " ++ atom_to_list(ModName))
	end.
	
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

%% Runs analysis on the supplied files using a modified version of the typer
%% Files is a list of files to process, 
%% Plt is the name of the Plt file.
handle_call({typer,FilesBeam,Plt,FilesErl,Outputmode}, _From, State) ->
	try	
		DialOpts = [{files,FilesBeam},{files_rec,[]},{include_dirs,[]},{output_plt,Plt},{defines,[]},{analysis_type,plt_build}],
		fileNameValid(FilesBeam),
%%		io:format("~nOptions: ~p~n",[dialyzer_options:build(DialOpts)]),
		_ListOfWarnings=dialyzer:run(DialOpts),
		Outcome = typer:start(FilesErl,Plt,Outputmode),
		{reply,{ok,Outcome}, State}
	catch
		error:Error -> {reply, {failed,[Error,erlang:get_stacktrace()]}, State}
	end;
	
%% Evaluates a term and returns a result, based on http://www.trapexit.org/String_Eval
handle_call({evaluateTerm,String}, _From, State) ->
	try	
		{ ok, Tokens, _ } = erl_scan:string(String),
		{ ok, Tree } = erl_parse:parse_exprs(Tokens),
		{ value, Value, NewBindings} = erl_eval:exprs(Tree,[]),
		{ reply, { ok, Value }, State }
	catch
		error:Error -> {reply, {failed,[Error,erlang:get_stacktrace()]}, State}
	end;
		

%% Compiles modules into .beam files, Dir is where to put results, should exist.
handle_call({compile,[],erlc,_Dir}, _From, State) ->
	{reply, ok, State};
	
handle_call({compile,[M | OtherModules],erlc,Dir}, From, State) ->
	case(compile:file(M,[verbose,debug_info,{outdir,Dir}])) of
		{ok,_} -> handle_call({compile,OtherModules,erlc,Dir}, From, State);
		_ -> {reply, failedToCompile, State}
	end;

%% Extracts dependencies from the supplied module
handle_call({dependencies,M}, _From, State) ->
	case(beam_lib:chunks(M,[imports])) of
		{ok,{_,[{imports,ImportList}]}} -> {reply, {ok,ImportList}, State};
		_ -> {reply, failed, State}
	end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test routines.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Used for testing - does not produce a response.
handle_call(noreply,_From, State) ->
	{noreply, State};

%% Used for testing - produces a specific response.
handle_call({echo,[Head | Tail]},_From, State) ->
	{reply, { Head,State#state.processNum, Tail }, State};

%% Used for testing - produces a specific response.
handle_call({echo2Tuple,aaa},_From, State) ->
	{reply, { ok, aaa, bbb }, State};

%% Used for testing - produces a specific response.
handle_call({echo2Notuple,aaa},_From, State) ->
	{reply, ok, State};

%% Used for testing - produces a specific response.
handle_call({echo2Error,aaa},_From, State) ->
	{reply, error, State};

%% Used for testing - produces a specific response.
handle_call({echo2ErrorMessage,aaa},_From, State) ->
	{reply, {error,veryLongErrorMessage}, State};
	
%% Used for testing - produces a specific response.
handle_call({echo2WrongType,aaa},_From, State) ->
	{reply, {[message]}, State};

%% Used for testing - produces a specific response.
handle_call({echo2List,aaa},_From, State) ->
	{reply, [ok,message], State};

%% Used for testing - produces a specific response.
handle_call({echo2ShortTuple,aaa},_From, State) ->
	{reply, {}, State}.
	
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

