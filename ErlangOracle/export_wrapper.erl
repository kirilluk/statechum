-module(export_wrapper).
-export([exec_call_trace/3]).

exec_call_trace(Module, [], OpProc) ->
    ok;
exec_call_trace(Module, [{Funcion, Args} | Trace], OpProc) ->
    OP = apply(Module, Function, Args),
    OpProc ! {self(), output, {Function, Args, OP}},
    exec_call_trace(Module, Trace, OpProc);
exec_call_trace(Module, [{Funcion, Args, OP} | Trace], OpProc) ->
    ThisOP = apply(Module, Function, Args),
    if (ThisOP =/= OP) ->
	    OpProc ! {self(), output_mismatch, {Function, Args, ThisOP}},
	    erlang:exit("Output mismatch");
       true ->
	    OpProc ! {self(), output, {Function, Args, OP}},
	    exec_call_trace(Module, Trace, OpProc)
    end.

    
