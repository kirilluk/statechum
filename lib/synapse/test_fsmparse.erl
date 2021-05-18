%%% -------------------------------------------------------------------
%%% Author  : Kirill
%%% Description : Runs traces on Erlang modules and reports results.
%%% Copyright (c) 2013 The University of Sheffield
%%% 
%%% This file is part of StateChum
%%% 
%%% StateChum is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%% 
%%% StateChum is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%% 
%%% You should have received a copy of the GNU General Public License
%%% along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%% This has to be run from within Statechum that will start Synapse before running this

-module(test_fsmparse).

-include_lib("eunit/include/eunit.hrl").
-include("synapse.hrl").


uploadtraces_test_() ->
	{"tests trace parsing",
	{inparallel,
	[
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,traces,a},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"Atom cannot be cast")) end end) end,
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[aaa]},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:cannotBeCast(Text,"Atom","com.ericsson.otp.erlang.OtpErlangTuple")) end end) end,
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[{pp,gg,eer}]},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"more than a pair of pos/neg")) end end) end,
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[{[],gg}]},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:cannotBeCast(Text,"List","com.ericsson.otp.erlang.OtpErlangAtom")) end end) end,
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[{pp,gg}]},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"instead of pos/neg")) end end) end,
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[{pos,gg}]},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:cannotBeCast(Text,"Atom","com.ericsson.otp.erlang.OtpErlangList")) end end) end,
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[{pos,["gg"]}]},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:cannotBeCast(Text,"String","com.ericsson.otp.erlang.OtpErlangAtom")) end end) end,

		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[ {pos,[a]}, aaa] },receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:cannotBeCast(Text,"Atom","com.ericsson.otp.erlang.OtpErlangTuple")) end end) end,
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[ {pos,[a]}, {pp,gg,eer}] },receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"more than a pair of pos/neg")) end end) end,
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[ {pos,[a]}, {[],gg}]},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:cannotBeCast(Text,"List","com.ericsson.otp.erlang.OtpErlangAtom")) end end) end,
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[ {pos,[a]}, {pp,gg}]},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"instead of pos/neg")) end end) end,
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[ {pos,[a]}, {pos,gg}]},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:cannotBeCast(Text,"Atom","com.ericsson.otp.erlang.OtpErlangList")) end end) end,
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[ {pos,[a]}, {pos,["gg"]}]},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:cannotBeCast(Text,"String","com.ericsson.otp.erlang.OtpErlangAtom")) end end) end,
		
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[{pos,[a,b,c]},{pos,[a,b,c]},{neg,[a]}]},receive {Ref,ok} -> Pid!{Ref,getTraces},receive {Ref,ok,Pos,Neg} ->?assertEqual("[[a, b, c]]",Pos),?assertEqual("[[a]]",Neg) end end end) end,
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[{pos,[a,b,c]},{pos,[a,b,c]},{neg,[a,a,a,a,a,a,aaa]},{neg,[a]}]},receive {Ref,ok} -> Pid!{Ref,getTraces},receive {Ref,ok,Pos,Neg} ->?assertEqual("[[a, b, c]]",Pos),?assertEqual("[[a], [a, a, a, a, a, a, aaa]]",Neg) end end end) end,
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[{pos,[]}]},receive {Ref,ok} -> Pid!{Ref,getTraces},receive {Ref,ok,Pos,Neg} ->?assertEqual("[[]]",Pos),?assertEqual("[]",Neg) end end end) end,
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[{neg,[]},{pos,[]}]},receive {Ref,ok} -> Pid!{Ref,getTraces},receive {Ref,ok,Pos,Neg} ->?assertEqual("[[]]",Pos),?assertEqual("[[]]",Neg) end end end) end,
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[]},receive {Ref,ok} -> Pid!{Ref,getTraces},receive {Ref,ok,Pos,Neg} ->?assertEqual("[]",Pos),?assertEqual("[]",Neg) end end end) end,
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,getTraces},receive {Ref,ok,Pos,Neg} ->?assertEqual("[]",Pos),?assertEqual("[]",Neg) end end) end
	]}}.

loadStatemachine_test_() ->
		{"tests FSM parsing",
	{inparallel,
	[
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,{
					  [a,b,c]
					  ,[{a,wibble,b},{b,wobble,c}]
					  ,a
					  ,[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"expected 5 components in FSM")) end end) end,

		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,{"a",
					  [a,b,c]
					  ,[{a,wibble,b},{b,wobble,c}]
					  ,a
					  ,[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:cannotBeCast(Text,"String","com.ericsson.otp.erlang.OtpErlangAtom")) end end) end,
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,{b,
					  [a,b,c]
					  ,[{a,wibble,b},{b,wobble,c}]
					  ,a
					  ,[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"first element of a record should be")) end end) end,

		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=["a",b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:cannotBeCast(Text,"String","com.ericsson.otp.erlang.OtpErlangAtom")) end end) end,


		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,"c"]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:cannotBeCast(Text,"String","com.ericsson.otp.erlang.OtpErlangAtom")) end end) end,

		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,"wobble"]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:cannotBeCast(Text,"String","com.ericsson.otp.erlang.OtpErlangAtom")) end end) end,

		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=['',b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"empty state name")) end end) end,

		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=['',wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"empty label")) end end) end,


		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[d]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"invalid source state")) end end) end,

		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{'',wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"invalid source state")) end end) end,

		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{"a",wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:cannotBeCast(Text,"String","com.ericsson.otp.erlang.OtpErlangAtom")) end end) end,

		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibbleA,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"unknown label")) end end) end,
					 
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobbleA,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"unknown label")) end end) end,

		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,'',c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"unknown label")) end end) end,

		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,"wobble",c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:cannotBeCast(Text,"String","com.ericsson.otp.erlang.OtpErlangAtom")) end end) end,

		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},aa]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:cannotBeCast(Text,"Atom","com.ericsson.otp.erlang.OtpErlangTuple")) end end) end,

		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{a,wobble,c,d}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"expected 3 components in transition")) end end) end,

		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,bB},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"invalid target state")) end end) end,
					 
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,''},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"invalid target state")) end end) end,

		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,"b"},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:cannotBeCast(Text,"String","com.ericsson.otp.erlang.OtpErlangAtom")) end end) end,

		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[]
					  ,transitions=[]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"empty automaton")) end end) end,

		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state="a"
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:cannotBeCast(Text,"String","com.ericsson.otp.erlang.OtpErlangAtom")) end end) end,

		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=''
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"missing initial state")) end end) end,

		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=d
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"missing initial state")) end end) end,

		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{a,wibble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"non-determinism detected")) end end) end,

		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,ok} -> Pid!{Ref,getFSM},receive {Ref,ok,#statemachine{
					  states=[a,b,c]
					  ,transitions=[]
					  ,initial_state=a
					  ,alphabet=[]
					 }} -> ok end end end) end,

		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,ok} -> Pid!{Ref,getFSM},receive {Ref,ok,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]}} -> ok end end end) end
	]}}.
					  
loadStatemachineRelaxed_test_() ->
		{"tests relaxed FSM parsing",
	{inparallel,
	[
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,testLoadFSM,#statemachine{
					  states=[]
					  ,transitions=[{a,wibble,b},{'',wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"empty source state")) end end) end,
					 
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,testLoadFSM,#statemachine{
					  states=[]
					  ,transitions=[{a,wibble,b},{b,wobble,''}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"empty target state")) end end) end,
					 
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,testLoadFSM,#statemachine{
					  states=[]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=''
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"empty initial state")) end end) end,
					 
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,testLoadFSM,#statemachine{
					  states=[]
					  ,transitions=[{a,wibble,b},{b,'',c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"empty label")) end end) end,
					 
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,testLoadFSM,#statemachine{
					  states=[]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,ok,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]}} -> ok end end) end,					  
					  
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,testLoadFSM,#statemachine{
					  states=[d]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,ok,#statemachine{
					  states=[d,a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]}} -> ok end end) end,

		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,testLoadFSM,#statemachine{
					  states=[d]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[]
					 }},receive {Ref,ok,#statemachine{
					  states=[d,a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]}} -> ok end end) end,
					  
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,testLoadFSM,#statemachine{
					  states=[d]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[junk]
					 }},receive {Ref,ok,#statemachine{
					  states=[d,a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]}} -> ok end end) end,
					  
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,testLoadFSM,#statemachine{
					  states=[d]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=e
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,ok,#statemachine{
					  states=[d,a,b,c,e]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=e
					  ,alphabet=[wibble,wobble]}} -> ok end end) end
					  					  
		%% The rest is tested with TestSynapse.testParseAutomata		  
			
	]}}.
	
