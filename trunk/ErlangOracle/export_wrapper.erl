-module(export_wrapper).
-export([exec_call_trace/3]).

%% Based on http://erlang.org/pipermail/erlang-questions/2007-April/026147.html

exec_call_trace(Module, Trace, OpProc) ->
    group_leader(OpProc, self()),
    call_trace(Module, Trace, OpProc).
    
call_trace(_Module, [], _OpProc) ->
    ok;
call_trace(Module, [{Function, Args} | Trace], OpProc) ->
 	try	
		OP = apply(Module, Function, Args),
		OpProc ! {self(), output, {Function, Args, OP}},
		call_trace(Module, Trace, OpProc)
	catch
		_Ecls:_Error -> OpProc ! {self(), failed, Function }
	end;
	
call_trace(Module, [{Function, Args, OP} | Trace], OpProc) ->
 	try	
		ThisOP = apply(Module, Function, Args),
		if (ThisOP =/= OP) ->
			OpProc ! {self(), output_mismatch, {Function, Args, ThisOP}};
		   true ->
			OpProc ! {self(), output, {Function, Args, OP}},
			call_trace(Module, Trace, OpProc)
		end
	catch
		_Ecls:_Error -> OpProc ! {self(), failed, Function }
	end.

    
