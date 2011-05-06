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

-include("tracerunner.hrl").

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
    {ok, #statechum{processNum=Arg}}.

%% Loads the supplied erl directly, can be used to substitute an arbitrary Erlang module with that of our own.
%% Invented to replace Typer modules, but since I had to replace the main module, this function is not used. 
compileAndLoad(What,Path) ->
	{ok,Bin,_}=compile:file(filename:join(Path,What),[verbose,debug_info,binary]),
	ModuleName = filename:basename(What,".erl"),
	code:purge(ModuleName),
	{module, _}=code:load_binary(ModuleName,"in_memory"++atom_to_list(What),Bin).

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
		false-> "Invalid file name " ++ F ++ " for module " ++ atom_to_list(ModName)
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
handle_call({runTrace,Module, Wrapper, Trace, ModulesList}, _From, State) ->
    	{Outcome, OPTrace, State3} = tracer3:first_failure(Module, Wrapper, Trace, ModulesList, State),
    	{reply, {ok, Outcome, OPTrace}, State3};

%% Adds the specified directory to the code path
handle_call({addPath,Path}, _From, State) ->
case(code:add_path(filename:join(filename:split(Path)))) of
	true -> {reply, ok, State};
	{error, What} -> {reply, { failed, What }, State}
end;

%% Removes the specified directory from the code path
handle_call({delPath,Path}, _From, State) ->
ConvertedPath = filename:join(filename:split(Path)),
case(code:del_path(ConvertedPath)) of
	true -> {reply, ok, State};
	false->{reply, { failed, ConvertedPath, code:get_path() }, State};
	{error, What} -> {reply, { failed, What }, State}
end;

%% Runs dialyzer on the supplied files.
%% Files is a list of files to process, 
%% Plt is the name of the Plt file.
handle_call({dialyzer,FilesBeam,Plt,_FilesErl,_Outputmode}, _From, State) ->
	try	
		DialOpts = [{files,FilesBeam},{files_rec,[]},{include_dirs,[]},{output_plt,Plt},{defines,[]},{analysis_type,plt_build}],
		case (fileNameValid(FilesBeam)) of
			ok ->
				_ListOfWarnings=dialyzer:run(DialOpts),{reply,ok, State};
			Error ->{reply, {failed,Error}, State}
		end
	catch
		error:_Error -> {reply, {failed,dialyzer_failed}, State}
	end;

%% Runs typer on the supplied files using a modified version of the typer,
%% assuming that Dialyzer has been run previously.
%% Files is a list of files to process, 
%% Plt is the name of the Plt file.
handle_call({typer,_FilesBeam,Plt,FilesErl,Outputmode}, _From, State) ->
	try	
		Outcome = typer:start(FilesErl,Plt,Outputmode),
		{reply,{ok,Outcome}, State}
	catch
		error:Error -> {reply, {failed,Error}, State}
	end;
%% [Error,erlang:get_stacktrace()]

%% Loads Statechum's configuration variables into this process.
handle_call({getAttr,Attr}, _From, State) ->
	case dict:find(Attr,State#statechum.attr) of
		{ok, Value} -> 	{reply, {ok, Value}, State};
		error ->	{reply, {failed, no_such_attribute}, State}
	end;

%% Obtains a value of a specific attribute		
handle_call({setConfiguration,Configuration}, _From, State) ->
	{reply, ok, State#statechum{config=dict:from_list(Configuration)}};

%% Evaluates a term and returns a result, based on http://www.trapexit.org/String_Eval
handle_call({evaluateTerm,String}, _From, State) ->
	try	
		{ ok, Tokens, _ } = erl_scan:string(String),
		{ ok, Tree } = erl_parse:parse_exprs(Tokens),
		{ value, Value, _NewBindings} = erl_eval:exprs(Tree,[]),
		{ reply, { ok, Value }, State }
	catch
		ErrClass:Error -> {reply, {failed,[ErrClass,Error,erlang:get_stacktrace()]}, State}
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

%% Extracts attributes from the supplied module
handle_call({attributes,M}, _From, State) ->
	case(beam_lib:chunks(M,[attributes])) of
		{ok,{_,[{attributes,AttributeList}]}} -> {reply, {ok,AttributeList}, State};
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
	{reply, { Head,State#statechum.processNum, Tail }, State};

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

