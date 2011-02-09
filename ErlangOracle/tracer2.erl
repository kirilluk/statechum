-module(tracer2).
-export([first_failure/4,gen_random_traces/4]).

%% Try Trace on Module:Function and report success or failure
%% Trace result is appended to OutFile, coverage map is appended to OutFile.covermap
first_failure(Module, Function, Trace, Remains, OutFile) ->
    TraceString = lists:foldl(fun(Elem, Acc) -> Acc ++ " " ++ atom_to_list(Elem) end, "", Trace),
    case check_file_for_trace(OutFile, TraceString) of
	ok ->
	    case Remains of
		[] ->
		    ok;
		_List ->
		    first_failure(Module, Function, Trace ++ [hd(Remains)], tl(Remains), OutFile)
	    end;
	failed ->
	    ok;
	not_found ->
	    {Status, CoverMap} = try_trace(Module, Function, Trace),
	    append_to_file(OutFile ++ ".covermap", io_lib:format("[]-~w => ~w", [Trace, CoverMap])),
	    case Status of
		failed ->
		    append_to_file(OutFile, io_lib:format("- ~s", [TraceString]));
		ok ->
		    append_to_file(OutFile, io_lib:format("+ ~s", [TraceString])),
		    case Remains of
			[] ->
			    ok;
			_List ->
			    first_failure(Module, Function, Trace ++ [hd(Remains)], tl(Remains), OutFile)
		    end
	    end
    end.

%% The public face of first failure....
first_failure(Module, Function, Trace, OutFile) ->
    first_failure(Module, Function, [], Trace, OutFile).

%% Run the specified function with the specified Trace as input
%% trap the resulting status and the code coverage
try_trace(Module, Function, Trace) ->
    cover:compile(Module),
    {Pid, Ref} = spawn_monitor(Module, Function, [Trace]),
    ProcStatus = await_end(Pid, Ref),
    demonitor(Ref),
    cover:analyse_to_file(Module, "tmp.cover", []),
    {ProcStatus, create_map("tmp.cover")}.

%%
%% Helper functions
%%
    
append_to_file(FileName, String) ->
    {ok, IODevice} = file:open(FileName, [append]),
    io:format(IODevice, "~s~n", [String]),
    file:close(IODevice).

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
	    %% Strip the closing \n
	    {TrueLineString, _Return} = lists:split(length(LineString)-1, LineString),
	    if 
		TrueLineString == TraceString -> 
		    if
			StatString == "+ " ->
			    ok;
			true ->
			    failed
		    end;
		true ->
		    check_lines(IODevice, TraceString)
	    end
    end.


generate_input_set(_, 0) ->
    [];
generate_input_set(Aleph, N) ->
    [gen_random_string(Aleph, random:uniform(200)) | generate_input_set(Aleph, N-1)].

gen_random_string(_Aleph, 0) ->
    [];
gen_random_string(Aleph, N) ->
     [lists:nth(random:uniform(length(Aleph)), Aleph) | gen_random_string(Aleph, N-1)]. 

gen_random_traces(Module, Function, Alphabet, OutFile) ->
    InputSet = generate_input_set(Alphabet, 100),
    try_all_traces(Module, Function, InputSet, OutFile).


try_all_traces(_Module, _Function, [], _OutFile) ->
    ok;
try_all_traces(Module, Function, [T | Traces], OutFile) ->
    first_failure(Module, Function, [], T, OutFile),
    try_all_traces(Module, Function, Traces, OutFile).

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
