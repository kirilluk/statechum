-module(gen_event_wrapper).
-export([exec_call_trace/3]).

%% Yes, this is a crazy signature. It lets us use the standard version of tracer2 on these modules that need InitArgs
exec_call_trace(Module, InitArgs, Trace) ->
    io:format("Executing gen_event:start_link({local, ~p}).~n", [Module]),
    {ok, Pid} = gen_event:start_link({local, Module}),
    ok = gen_event:add_handler(Module, Module, InitArgs),
    %%Module:init(InitArgs),
    Output = call_trace({mod_under_test, Pid}, Trace, []),
    %%Module:terminate(stop, who_cares_state),
    gen_event:stop(Module),
    Output.

call_trace(_ModulePid, [], Output) ->
    Output;
call_trace({Module, Pid}, [{event, T} | Trace], Output) ->
io:format("Executing gen_event:sync_notify(~p, ~p, 500)).~n", [Module, T]),
    OP = gen_event:sync_notify(Module, T, 500),
    call_trace({Module, Pid}, Trace, [OP | Output]);
call_trace({Module, Pid}, [{call, T} | Trace], Output) ->
io:format("Executing gen_event:call(~p, ~p, 500)).~n", [Module, T]),
    OP = gen_event:call(Module, T, 500),
    call_trace({Module, Pid}, Trace, [OP | Output]);
call_trace({Module, Pid}, [{info, T} | Trace], Output) ->
io:format("Executing gen_server:callinfo (~p, ~p, 500)).~n", [Module, T]),
    Pid ! T,
    receive 
	Msg ->
	    OP = Msg
    after 500 ->
	    OP = {timeout, T}
    end,
    call_trace({Module, Pid}, Trace, [OP | Output]).
