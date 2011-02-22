-module(gen_fsm_wrapper).
-export([exec_call_trace/2]).

%% Yes, this is a crazy signature. It lets us use the standard version of tracer2 on these modules that need InitArgs
exec_call_trace(Module, [{init, InitArgs} | Trace]) ->
    io:format("Executing gen_fsm:start_link({local, mod_under_test}, ~p, ~p, []).~n", [Module, InitArgs]),
    {ok, Pid} = gen_fsm:start_link({local, mod_under_test}, Module, InitArgs, []),
    %%Module:init(InitArgs),
    Output = call_trace({mod_under_test, Pid}, Trace, []),
    %%Module:terminate(stop, who_cares_state),
    gen_fsm:send_event(mod_under_test, stop),
    Output.

call_trace(_ModulePid, [], Output) ->
    Output;
call_trace({Module, Pid}, [{sync, T} | Trace], Output) ->
    OP = gen_fsm:sync_send_event(Module, T, 500),
    call_trace({Module, Pid}, Trace, [OP | Output]);
call_trace({Module, Pid}, [{event, T} | Trace], Output) ->
    OP = gen_fsm:send_event(Module, T, 500),
    call_trace({Module, Pid}, Trace, [OP | Output]);
call_trace({Module, Pid}, [{info, T} | Trace], Output) ->
    Pid ! T,
    receive 
	Msg ->
	    OP = Msg
    after 500 ->
	    OP = {timeout, T}
    end,
    call_trace({Module, Pid}, Trace, [OP | Output]).
