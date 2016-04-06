-module(export_wrapper).
-export([exec_call_trace/4]).

%% Based on http://erlang.org/pipermail/erlang-questions/2007-April/026147.html

exec_call_trace(Module, Trace, OpProc,Delay) ->
    group_leader(OpProc, self()),
    call_trace(Module, Trace, OpProc,Delay).
    
call_trace(_Module, [], _OpProc,_Delay) ->
    ok;
call_trace(Module, [{Function, Args} | Trace], OpProc,Delay) ->
 	try	
		OP = apply(Module, Function, Args),
		OpProc ! {self(), output, {Function, Args, OP}},
		call_trace(Module, Trace, OpProc,Delay)
	catch
		_Ecls:_Error -> OpProc ! {self(), failed, Function }
	end;
	
call_trace(Module, [{Function, Args, OP} | Trace], OpProc,Delay) ->
 	try	
		ThisOP = apply(Module, Function, Args),
		if (ThisOP =/= OP) ->
			OpProc ! {self(), output_mismatch, {Function, Args, ThisOP}};
		   true ->
			OpProc ! {self(), output, {Function, Args, OP}},
			call_trace(Module, Trace, OpProc,Delay)
		end
	catch
		_Ecls:_Error -> OpProc ! {self(), failed, Function }
	end.

    
