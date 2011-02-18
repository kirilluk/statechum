-module(gen_server_wrapper).
-export([exec_call_trace/3]).

%% Yes, this is a crazy signature. It lets us use the standard version of tracer2 on these modules that need InitArgs
exec_call_trace(Module, InitArgs, Trace) ->
    io:format("Executing gen_server:start_link({local, mod_under_test}, ~p, ~p, []).~n", [Module, InitArgs]),
    {ok, Pid} = gen_server:start_link({local, mod_under_test}, Module, InitArgs, []),
    %%Module:init(InitArgs),
    Output = call_trace({mod_under_test, Pid}, Trace, []),
    %%Module:terminate(stop, who_cares_state),
    gen_server:cast(Pid, stop),
    Output.

call_trace(_ModulePid, [], Output) ->
    Output;
call_trace({Module, Pid}, [{call, T} | Trace], Output) ->
io:format("Executing gen_server:call(~p, ~p, 500)).~n", [Module, T]),
    OP = gen_server:call(Module, T, 500),
    call_trace({Module, Pid}, Trace, [OP | Output]);
call_trace({Module, Pid}, [{info, T} | Trace], Output) ->
    Pid ! T,
    receive 
	Msg ->
	    OP = Msg
    after 500 ->
	    OP = {timeout, T}
    end,
    call_trace({Module, Pid}, Trace, [OP | Output]);
call_trace({Module, Pid}, [{cast, T} | Trace], Output) ->
    OP = gen_server:cast(Module, T),
    call_trace({Module, Pid}, Trace, [OP | Output]).



