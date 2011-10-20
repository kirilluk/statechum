-module(gen_fsm_wrapper).
-export([exec_call_trace/3]).

exec_call_trace(Module, [{init, InitArgs, OP} | Trace], OpProc) ->
    %%io:format("Executing gen_server:start_link({local, mod_under_test}, ~p, ~p, []).~n", [Module, InitArgs]),
    {ok, Pid} = gen_fsm:start_link({local, mod_under_test}, Module, InitArgs, []),
    %%Module:init(InitArgs),
    if (ok =/= OP) ->
	    OpProc ! {self(), output_mismatch, {init, InitArgs, ok}},
    	    %%Module:terminate(stop, who_cares_state),
	    erlang:exit("Output mismatch");
      true ->
	    OpProc ! {self(), output, {init, InitArgs, ok}},
	    ok = call_trace({mod_under_test, Pid}, Trace, OpProc)
   
    end;

exec_call_trace(Module, [{init, InitArgs} | Trace], OpProc) ->
    %%io:format("Executing gen_server:start_link({local, mod_under_test}, ~p, ~p, []).~n", [Module, InitArgs]),
    {ok, Pid} = gen_fsm:start_link({local, mod_under_test}, Module, InitArgs, []),
    %%Module:init(InitArgs),
    OpProc ! {self(), output, {init, InitArgs, ok}},
    ok = call_trace({mod_under_test, Pid}, Trace, OpProc);

    %%OpProc ! {self(), output, stop};
exec_call_trace(_Module, [], _OpProc) ->
    ok;
exec_call_trace(_Module, _TraceNoInitArgs, _OpProc) ->
    erlang:exit("Trace with no init!").

call_trace(_ModulePid, [], _OpProc) ->
    ok;
%% Calling inits after initialisation is always bad...
call_trace({_Module, _Pid}, [{init, _T} | _Trace], _OpProc) ->
    erlang:exit("Init inside trace!");
%% This will accept any Output but records it in the written trace
call_trace({Module, Pid}, [{sync, T} | Trace], OpProc) ->
    OP = gen_fsm:sync_send_event(Module, T, 500),
    OpProc ! {self(), output, {sync, T, OP}},
    call_trace({Module, Pid}, Trace, OpProc);
call_trace({Module, Pid}, [{sync, T, OP} | Trace], OpProc) ->
    ThisOP = gen_fsm:sync_send_event(Module, T, 500),
    if (ThisOP =/= OP) ->
	    OpProc ! {self(), output_mismatch, {sync, T, ThisOP}},
	    erlang:exit("Output mismatch");
      true ->
	    OpProc ! {self(), output, {sync, T, ThisOP}},
	    call_trace({Module, Pid}, Trace, OpProc)
    end;
call_trace({Module, Pid}, [{event, T} | Trace], OpProc) ->
    OP = gen_fsm:send_event(Module, T, 500),
    OpProc ! {self(), output, {event, T, OP}},
    call_trace({Module, Pid}, Trace, OpProc);
call_trace({Module, Pid}, [{event, T, OP} | Trace], OpProc) ->
    ThisOP = gen_fsm:send_event(Module, T, 500),
    if (ThisOP =/= OP) ->
	    OpProc ! {self(), output_mismatch, {event, T, ThisOP}},
	    erlang:exit("Output mismatch");
      true ->
	    OpProc ! {self(), output, {event, T, ThisOP}},
	    call_trace({Module, Pid}, Trace, OpProc)
    end;
call_trace({Module, Pid}, [{info, T} | Trace],OpProc) ->
    Pid ! T,
    receive 
	Msg ->
	    _OP = Msg
    after 500 ->
	    _OP = {timeout, T}
    end,
    OpProc ! {self(), output, {info, T}},
    call_trace({Module, Pid}, Trace, OpProc).


