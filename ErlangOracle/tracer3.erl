-module(tracer3).
-export([first_failure/5]).

-include("tracerunner.hrl").

%% Module - Name of the Module Under Test
%% Wrapper - Name of the wrapper module - usually gen_server_wrapper, gen_fsm_wrapper etc.
%% Trace - List of operation tuples to try
%% ModulesList - List of Modules to cover compile for code coverage analysis
%% Options configuration to use.
first_failure(_Module, _Wrapper, [], _ModulesList, State) ->
    {ok, [],State};
first_failure(Module, Wrapper, Trace, ModulesList, #statechum{}=State) ->
    State2=compileModulesForAnalyser(ModulesList,State),
    {Pid, Ref} = spawn_monitor(Wrapper, exec_call_trace, [Module, Trace, self()]),
    {ProcStatus, PartialOPTrace} = await_end(Pid, Ref),
    erlang:demonitor(Ref,[flush]),
    OPTrace = flushOPTrace(PartialOPTrace, Pid, getConfig(?erlFlushDelay,State2)),
    %%io:format("~p >>>> ~p~n", [ProcStatus, OPTrace]),
    State3=analyse_all(ModulesList,State2),
    case ProcStatus of 
	ok ->
	    {ok, OPTrace, State3};
	failed_but ->
	    {failed_but, OPTrace, State3};
	failed ->
	    {failed, OPTrace, State3}
    end.
    
%%
%% Helper functions
%%
    
await_end(Pid, Ref) ->
    await_end(Pid, Ref, []).

await_end(Pid, Ref, OpTrace) ->
    receive
	{'DOWN', Ref, _X, _Y, normal} ->
	    {ok, OpTrace};
	{'EXIT', Ref, _X, _Y, _Status} ->
	    {failed, OpTrace};
	{'DOWN', Ref, _X, _Y, _Status} ->
	    {failed, OpTrace};
	{Pid, output, OP} ->
	    await_end(Pid, Ref, OpTrace ++ [OP]);
	{Pid, output_mismatch, OP} ->
	    {failed_but, OpTrace ++ [OP]};
	{Pid, failed, OP} ->
	    {failed, OpTrace ++ [OP]}
    after 100 ->    
	    case lists:member(Pid, erlang:processes()) of
		false ->
		    %%io:format("Scone...~n"),
		    {ok, OpTrace};
		true ->	    
		    await_end(Pid, Ref, OpTrace)
	    end
    end.

flushOPTrace(OPTrace, Pid, Delay) ->
  if 
    Delay =< 0 -> OPTrace;
    true ->
    receive
	{Pid, output, OP} ->
	    flushOPTrace(OPTrace ++ [OP], Pid, Delay);
	Msg ->
	    io:format("UNHANDLED: ~p~n", [Msg]),
	    flushOPTrace(OPTrace, Pid, Delay)
    after Delay ->
	    OPTrace
    end
  end.


%% Extracts the value of a variable from a configuration
getConfig(Var,#statechum{config=Conf}=_State) ->
	case(dict:find(Var,Conf)) of
		{ok,Value} -> Value;
		_ -> erlang:error("unknown configuration variable " ++ atom_to_list(Var))
	end.


%% Compiles all supplied modules for Analyser if coverage analysis is enabled.
%% and Modules which have already been compiled are recorded and not recompiled later.
compileModulesForAnalyser(ModulesList,State) ->
	CovValue = getConfig(?erlCoverage,State),
	if 
		CovValue =:= 'ERLCOV_NONE' -> State;
		true	-> compileModulesA(ModulesList,State)
	end.

%% The worker method actually doing compilation
compileModulesA([], State) ->
	{reply, ok, State};

compileModulesA([M | OtherModules], State) ->
	case sets:is_element(M,State#statechum.compiledModules) of
		true -> compileModulesA(OtherModules, State);
		false->
			case(cover:compile(M)) of
				{ok,_} -> compileModulesA(OtherModules, 
					State#statechum{compiledModules=sets:add_element(M,State#statechum.compiledModules)});
				FailureDetails -> {reply, {failed, FailureDetails}, State}
			end
	end.

analyse_all(ModulesList,#statechum{config=Conf,attr=Attrs}=State) ->
	case(dict:find(?erlCoverage,Conf)) of
		{ok,'ERLCOV_LINE'} -> 
			AttrsWithCoverage = dict:store(coverage,analyse_linecoverage(ModulesList),Attrs),
			State#statechum{attr=AttrsWithCoverage};
		{ok,'ERLCOV_NONE'} -> State;
		{ok,Other} -> erlang:error("invalid coverage type " ++ atom_to_list(Other));
		_ -> erlang:error("coverage kind not defined")
	end.

analyse_linecoverage(ModulesList) ->
	lists:foldl(fun (Mod,Acc) ->
		{ok, Answer}=cover:analyse(Mod,line),
		Acc ++ Answer
		end, ModulesList, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Analysis via file

analyse_all_f([], Map) ->
    Map;
analyse_all_f([M | ModulesList], Map) ->
    cover:analyse_to_file(M, "tmp.cover", []),
    ThisMap = create_map("tmp.cover"),
    analyse_all_f(ModulesList, map_label(M, ThisMap) ++ Map).

create_map(FileName) ->
    {ok, IODevice} = file:open(FileName, [read]),
    %% Four lines of headers...
    map_lines(IODevice, [], -3).

map_lines(IODevice, Accum, LineNum) ->
    case io:get_line(IODevice, "") of
        eof  -> file:close(IODevice), Accum;
        Line -> NewAccum = process_line(Line, Accum, LineNum),
                    map_lines(IODevice, NewAccum, LineNum+1)
    end.

process_line(Line, Accum, LineNum) ->
    Toks = string:tokens(Line, ".."),
    Z = string:strip(hd(Toks)),
    case string:to_integer(Z) of
	{error, _Reason} ->
	    Accum;
	{Int, _Rest} ->
	    if (Int > 0) ->
		    Accum ++ [{LineNum, Int}];
	       true ->
		    Accum
	    end
    end.

map_label(_Label, []) ->
    [];
map_label(Label, [{Line, Count} | Map]) ->
    [{lists:flatten(io_lib:format("~w.~w", [Label, Line])), Count} | map_label(Label, Map)].

