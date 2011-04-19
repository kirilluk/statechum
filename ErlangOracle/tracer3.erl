-module(tracer3).
-export([first_failure/3, first_failure/4, trace_server/0, trace_server_loop/0, test_trace_server/3]).

%% server main loop that will receive and process questions from the Java components
trace_server() ->
    Pid = spawn(?MODULE, trace_server_loop, []),
    register(trace_server, Pid).

trace_server_loop() ->
    receive
	kill ->
	    ok;
	{first_failure, {Module, Wrapper, Trace}, From} ->
	    From ! first_failure(Module, Wrapper, Trace),
	    trace_server_loop();
	{first_failure, {Module, Wrapper, Trace, ModulesList}, From} ->
	    From ! first_failure(Module, Wrapper, Trace, ModulesList),
	    trace_server_loop()
    end.

test_trace_server(Module, Wrapper, Trace) ->
    trace_server ! {first_failure, {Module, Wrapper, Trace}, self()},
    receive
	Msg ->
	     Msg
    end.

%% Module - Name of the Module Under Test
%% Wrapper - Name of the wrapper module - usually gen_server_wrapper, gen_fsm_wrapper etc.
%% Trace - List of operation tuples to try
%% ModulesList - List of Modules to cover compile for code coverage analysis
first_failure(_Module, _Wrapper, [], _ModulesList) ->
    {ok, []};
first_failure(Module, Wrapper, Trace, ModulesList) ->
    compile_all(ModulesList),
    {Pid, Ref} = spawn_monitor(Wrapper, exec_call_trace, [Module, Trace, self()]),
    {ProcStatus, PartialOPTrace} = await_end(Pid, Ref),
    erlang:demonitor(Ref,[flush]),
    OPTrace = flushOPTrace(PartialOPTrace, Pid),
    %%io:format("~p >>>> ~p~n", [ProcStatus, OPTrace]),
    Coverage = analyse_all(ModulesList),
    case ProcStatus of 
	ok ->
	    {ok, OPTrace, Coverage};
	failed_but ->
	    {failed, OPTrace, Coverage};
	failed ->
	    {failed, OPTrace, Coverage}
    end.
    
first_failure(Module, Wrapper, Trace) ->
    first_failure(Module, Wrapper, Trace, [Module]).

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
    after 500 ->    
	    case lists:member(Pid, erlang:processes()) of
		false ->
		    %%io:format("Scone...~n"),
		    {ok, OpTrace};
		true ->	    
		    await_end(Pid, Ref, OpTrace)
	    end
    end.

flushOPTrace(OPTrace, Pid) ->
    receive
	{Pid, output, OP} ->
	    flushOPTrace(OPTrace ++ [OP], Pid);
	Msg ->
	    io:format("UNHANDLED: ~p~n", [Msg]),
	    flushOPTrace(OPTrace, Pid)
    after 500 ->
	    OPTrace
    end.

%% Code coverage functions
compile_all([]) ->
    ok;
compile_all([M | ModulesList]) ->
    cover:compile(M),
    compile_all(ModulesList).

analyse_all(ModulesList) ->
    analyse_all(ModulesList, []).

analyse_all([], Map) ->
    Map;
analyse_all([M | ModulesList], Map) ->
    cover:analyse_to_file(M, "tmp.cover", []),
    ThisMap = create_map("tmp.cover"),
    analyse_all(ModulesList, map_label(M, ThisMap) ++ Map).

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

