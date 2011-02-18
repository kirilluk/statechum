-module(tracer2).
-export([first_failure/5, first_failure/6, gen_random_traces/5, gen_random_traces/6]).

%% Try Trace on Module:Function and report success or failure
%% Trace result is appended to OutFile, coverage map is appended to OutFile.covermap
first_failure(WrapperModule, Module, Args, Trace, Remains, OutFile, ModulesList) ->
    TraceString = lists:foldl(fun(Elem, Acc) -> io_lib:format("~s ~w", [Acc, Elem]) end, "", Trace),
    {Status, CoverMap} = try_trace(WrapperModule, Module, Args, Trace, ModulesList),
    CoverMapString = map_to_string(CoverMap),
    append_to_file(OutFile ++ ".covermap", io_lib:format("[]-~w => [~s]", [Trace, CoverMapString])),
    case Status of
	failed ->
	    append_to_file(OutFile, io_lib:format("- ~s", [TraceString]));
	ok ->
	    append_to_file(OutFile, io_lib:format("+ ~s", [TraceString])),
	    case Remains of
		[] ->
		    ok;
		_List ->
		    first_failure(WrapperModule, Module, Args, Trace ++ [hd(Remains)], tl(Remains), OutFile, ModulesList)
	    end
    end.

%% The public face of first failure....
first_failure(WrapperModule, Module, Args, Trace, OutFile) ->
    first_failure(WrapperModule, Module, Args, Trace, OutFile, [Module]).

first_failure(WrapperModule, Module, Args, Trace, OutFile, ModulesList) ->
    %% First, lets make sure we don't already have a negative prefix
    %% If there is a positive prefix we can move on from that..
    {Status, Prefix} = find_prefix(OutFile, Trace),
    io:format("{~p, ~p}~n", [Status, Prefix]),
    case Status of
	ok ->
	    %% Positive prefix
	    if ((length(Prefix)+1) > length(Trace)) ->
		    %% Nothing further to add...
		    ok;
	       true ->
		    {NewPrefix, Suffix} = lists:split(length(Prefix)+1, Trace),
		    %%io:format("Extending from ~p with ~p~n", [NewPrefix, Suffix]),
		    first_failure(WrapperModule, Module, Args, NewPrefix, Suffix, OutFile, ModulesList)
	    end;
	failed ->
	    %% Negative prefix, nothing to do
	    ok;
	not_found ->
	    %% No data - do a complete run
	    io:format("No data for ~p~n", [Trace]),
	    first_failure(WrapperModule, Module, Args, [], Trace, OutFile, ModulesList)
    end.

%% Run the specified function with the specified Trace as input
%% trap the resulting status and the code coverage
try_trace(WrapperModule, Module, Args, Trace, ModulesList) ->
    io:format("Trying ~p:exec_call_trace(~p, ~p, ~p)...~n", [WrapperModule, Module, Args, Trace]),
    compile_all(ModulesList),
    {Pid, Ref} = spawn_monitor(WrapperModule, exec_call_trace, [Module, Args, Trace]),
    ProcStatus = await_end(Pid, Ref),
    erlang:demonitor(Ref,[flush]),
    {ProcStatus, analyse_all(ModulesList)}.

%%
%% Helper functions
%%
    
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

%% Prepend the module name to the line numbers...
map_label(_Label, []) ->
    [];
map_label(Label, [{Line, Count} | Map]) ->
    [{lists:flatten(io_lib:format("~w.~w", [Label, Line])), Count} | map_label(Label, Map)].

append_to_file(FileName, String) ->
    %%io:format("   ~s << ~s~n", [FileName, String]),
    {ok, IODevice} = file:open(FileName, [append]),
    io:format(IODevice, "~s~n", [String]),
    file:sync(IODevice),
    file:close(IODevice).


map_to_string([]) ->
    "";
map_to_string([{Line, Count} | Map]) ->
    This = lists:flatten(io_lib:format("{~s,~w}", [Line, Count])),
    Rest = map_to_string(Map),
    case Rest of 
	"" ->
	    This;
	_List ->
	    This ++ "," ++ Rest
    end.

await_end(Pid, Ref) ->
    case lists:member(Pid, erlang:processes()) of
	false ->
	    ok;
	true ->
	    receive
		{'DOWN', Ref, _, _, normal} ->
		    ok;
		{'EXIT', Ref, _X, _Y, _Status} ->
		    failed;
		{'DOWN', Ref, _X, _Y, _Status} ->
		    failed
	    after 100 ->
		      await_end(Pid, Ref)
	    end
    end.

check_file_for_trace(FileName, TraceString) ->
    {Status, IODevice} = file:open(FileName, [read]),
    case Status of 
	ok ->
	    ProcStatus = check_lines(IODevice, TraceString),
	    file:close(IODevice),
	    ProcStatus;
	_ ->
	    not_found
    end.

check_lines(IODevice, TraceString) ->
    case io:get_line(IODevice, "") of
	eof ->
	    not_found;
	Line ->
	    {StatString, LineString} = lists:split(2, Line),
	    %% Strip the closing " \n"
	    if 
		(length(LineString) == 0) ->
		    %% empty input line...
		    TrueLineString = "";
		true ->
		    {TrueLineString, _Return} = lists:split(length(LineString)-1, LineString)
	    end,
	    %%io:format("Seeking \"~p\" in \"~p\" (~p)~n", [TraceString, TrueLineString, string:equal(TrueLineString, TraceString)]),	    
	    case string:equal(TrueLineString, TraceString) of
		true -> 
		    if
			StatString == "+ " ->
			    ok;
			true ->
			    failed
		    end;
		false ->
		    check_lines(IODevice, TraceString)
	    end
    end.


find_prefix(OutFile, []) ->
    {check_file_for_trace(OutFile, ""), []};
find_prefix(OutFile, Trace) ->
    TraceString = lists:flatten(lists:foldl(fun(Elem, Acc) -> io_lib:format("~s ~w", [Acc, Elem]) end, "", Trace)),
    case check_file_for_trace(OutFile, TraceString) of
	not_found ->
	    {Prefix, _Suffix} = lists:split(length(Trace)-1, Trace),
	    find_prefix(OutFile, Prefix);
	Status ->
	    {Status, Trace}
    end.

%% Generates an input set with no repeated sequences, 200 is the number of attempts to generate a sequence.
%% The total number of seq will be lower than N if any attempt fails.
generate_input_set(_, 0, ExistingSeqList) ->
    ExistingSeqList;
generate_input_set(Aleph, N, ExistingSeqList) ->
 	NewSeq = generate_input_set_N_attempts(Aleph, N, ExistingSeqList, 200),
	if
		length(NewSeq) > 0 -> generate_input_set(Aleph, N-1,[ NewSeq | ExistingSeqList ]);
		true -> generate_input_set(Aleph, N-1,ExistingSeqList)
	end.

generate_input_set_N_attempts(Aleph, N, ExistingSeqList, Attempt) ->
	NewSeq = gen_random_string(Aleph, random:uniform(5)),
	AlreadyPresent = lists:member(NewSeq,ExistingSeqList),
	if 
		not AlreadyPresent -> NewSeq;
		Attempt > 0 -> generate_input_set_N_attempts(Aleph, N, ExistingSeqList,Attempt-1);
		true -> []
	end.

gen_random_string(_Aleph, 0) ->
    [];
gen_random_string(Aleph, N) ->
     [lists:nth(random:uniform(length(Aleph)), Aleph) | gen_random_string(Aleph, N-1)]. 

gen_random_traces(WrapperModule, Module, InitArgs, Alphabet, OutFile, ModuleList) ->
    InputSet = lists:sort(generate_input_set(Alphabet, 20, [])),
    try_all_traces(WrapperModule, Module, InitArgs, InputSet, OutFile, ModuleList).

gen_random_traces(WrapperModule, Module, InitArgs, Alphabet, OutFile) ->
    gen_random_traces(WrapperModule, Module, InitArgs, Alphabet, OutFile, [Module]).

try_all_traces(_WrapperModule, _Module, _InitArgs, [], _OutFile, _ModuleList) ->
    ok;
try_all_traces(WrapperModule, Module, InitArgs, [T | Traces], OutFile, ModuleList) ->
    first_failure(WrapperModule, Module, InitArgs, T, OutFile, ModuleList),
    try_all_traces(WrapperModule, Module, InitArgs, Traces, OutFile, ModuleList).

%%
%% Coverage functions
%%

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
