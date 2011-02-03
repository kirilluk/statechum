-module(tracer).
-export([trace/4, first_failure/4, gen_random_traces/4, gen_max_coverage_traces/4, await_end/2]).

first_failure(Module, Fun, Input, OutFile) ->
    first_failure(Module, Fun, Input, OutFile, []).    

first_failure(Module, Fun, Input, OutFile, Previous) ->
    %% check the prefix is not already in the set...
    PrefixFound = lists:foldl(fun(Elem, Acc) -> lists:prefix(Elem, Input) or Acc end, false, Previous),
    case PrefixFound of
	true ->
	    {ok, io_lib:format("# Skipping ~p", [Input])};
	false ->
	    io:format("Trying input of length ~p~n", [length(Input)]),
	    case Input of
		[] ->
		    %% pretend it works to queue the rest of the writing...
		    ProcStatus = ok;
		_ ->
		    process_flag(trap_exit, true),
		    %%cover:compile(Module),
		    {Pid, Ref} = spawn_monitor(Module, Fun, [Input]),
		    ProcStatus = await_end(Pid, Ref),
		    demonitor(Ref)
		    %%cover:analyse_to_file(Module, OutFile ++ ".cover", []),
		    %%cover:analyse_to_file(Module, OutFile ++ ".cover.html", [html])
	    end,
	    case ProcStatus of
		failed ->
		    Trace = "- " ++ lists:foldl(fun(Elem, Acc) -> Acc ++ " " ++ atom_to_list(Elem) end, "", Input),
		    case Input of
			[] ->
			    Trace;
			_ ->
			    %% Try to shorten trace until it works...
			    {IHead, _ITail} = lists:split(length(Input)-1, Input),
			    {Status, SubInput, SubTrace} = first_failure(Module, Fun, IHead, OutFile),
			    case Status of
				ok ->
				    %% We are the first failure...
				    write_traces([Trace], OutFile, write),
				    {failed, Input, Trace};
				failed ->
				    %% its ok, someone lower will have writen the file...
				    {failed, SubInput, SubTrace}
			    end
		    end;
		ok ->
		    Trace = "+ " ++ lists:foldl(fun(Elem, Acc) -> Acc ++ " " ++ atom_to_list(Elem) end, "", Input),
		    write_traces([Trace], OutFile, write),
		    {ok, Input, Trace}
	    end
    end.

trace(Module, Fun, Input, OutFile) ->
    process_flag(trap_exit, true),
    %%cover:compile(Module),
    {Pid, Ref} = spawn_monitor(Module, Fun, [Input]),
    ProcStatus = await_end(Pid, Ref),
    demonitor(Ref),
    %%cover:analyse_to_file(Module, OutFile ++ ".cover", []),
    %%cover:analyse_to_file(Module, OutFile ++ ".cover.html", [html]),
    case ProcStatus of
	failed ->
	    Trace = "- " ++ lists:foldl(fun(Elem, Acc) -> Acc ++ " " ++ atom_to_list(Elem) end, "", Input),
	    case Input of
		[] ->
		    Trace;
		_List ->
		    %% Try to shorten trace until it works...
		    {IHead, _ITail} = lists:split(length(Input)-1, Input),
		    trace(Module, Fun, IHead, OutFile)
	    end;
	ok ->
	    Trace = "+ " ++ lists:foldl(fun(Elem, Acc) -> Acc ++ " " ++ atom_to_list(Elem) end, "", Input)
    end,
    {Status, IODevice} = file:open(OutFile, [read]),
    case Status of
	ok ->
	    Found = check_for_trace(IODevice, Trace),
	    file:close(IODevice);
	error ->
	    case IODevice of
		enoent ->
		    %% file doesn't exist...
		    Found = false;
		Reason ->
		    Found = true,
		    io:format("Error opening output file: ~p~n", [Reason])
	    end
    end,
    case Found of 
	true ->
	    ok;
	false ->
	    write_traces([Trace], OutFile, append)
    end.

write_all(_IODevice, []) ->
    ok;
write_all(IODevice, [T | Traces]) ->
    io:format(IODevice, "~s~n", [T]),
    write_all(IODevice, Traces).

write_traces(Traces, OutFile, Mode) ->
	    {Status2, IODevice2} = file:open(OutFile, [Mode]),
	    case Status2 of
		ok ->
		    write_all(IODevice2, Traces),
		    file:close(IODevice2);
		error ->
		    io:format("Error opening output file: ~p~n", [IODevice2])
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

get_tagged_response(Tag) ->
    receive
	{Tag, Res} ->
	    Res
    end.

check_for_trace(IODevice, Trace) ->
    Status = file:read_line(IODevice),
    case Status of 
	{ok, Data} ->
	    TraceLine = Trace ++ "\n",
	    if (Data == TraceLine) ->
		    true;
	       true ->
		    check_for_trace(IODevice, Trace)
	    end;
	eof ->
	    false;
	{error, Reason} ->
	    erlang:exit(Reason)
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
    InputSet = generate_input_set(Alphabet, 1000),
    create_traces_file(Module, Function, InputSet, OutFile).

create_traces(_Module, _Function, [], _OutFile) ->
    [];
create_traces(Module, Function, [I | InputSet], OutFile) ->
    {Status, Trace, TraceString} = first_failure(Module, Function, I, OutFile),
    case Status of
	failed ->
	    NewInputSet = lists:filter(fun(Elem) -> not lists:prefix(Trace, Elem) end, InputSet);
	ok ->
	    NewInputSet = InputSet
    end,
    [TraceString | create_traces(Module, Function, NewInputSet, OutFile)].

create_traces_file(Module, Function, InputSet, OutFile) ->
    Traces = create_traces(Module, Function, InputSet, OutFile),
    %%write_traces([io_lib:format("# Used Input Set ~p", [InputSet]) | Traces], OutFile, write).
    write_traces(Traces, OutFile, write).

gen_max_coverage_traces(Module, Function, Alphabet, OutFile) ->  
    {_Model, InputSet} = inferer2:infer(Module, Function, Alphabet, 4),
    create_traces_file(Module, Function, InputSet, OutFile).
    
