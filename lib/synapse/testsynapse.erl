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

-module(testsynapse).

-include_lib("eunit/include/eunit.hrl").
-include("synapse.hrl").

useworker(Function) ->
	StatechumRef=make_ref(),synapselauncher:find_statechum()!{self(),StatechumRef,getStatechumWorker},receive {StatechumRef,Pid} -> Ref = make_ref(),Function(Pid,Ref), Pid!{Ref,terminate} end.

%% @edoc checks whether the second string is contained in the first one
containsStrict(List,[])->true;
containsStrict([],List)->false;
containsStrict([H|T],[A|B]) when H =:= A ->containsStrict(T,B);
containsStrict([H|T],[A|B]) when H =/= A ->false.

contains(List,[]) -> true;
contains([],List) -> false;
contains([H|T],[A|B]) when H=:=A -> containsStrict(T,B) or contains(T,[A|B]);
contains([H|T],[A|B]) when H=/=A -> contains(T,[A|B]).


contains_test_() ->
	[
		?_assertEqual(true,containsStrict("","")),
		?_assertEqual(true,containsStrict("abc","")),
		?_assertEqual(true,containsStrict("abc","a")),
		?_assertEqual(false,containsStrict("abc","b")),
		?_assertEqual(false,containsStrict("abc","c")),
		?_assertEqual(false,containsStrict("abc","bc")),
		?_assertEqual(true,containsStrict("abc","ab")),
		?_assertEqual(true,containsStrict("abc","abc")),

		?_assertEqual(false,containsStrict("abc","d")),
		?_assertEqual(false,containsStrict("abc","ac")),
		?_assertEqual(false,containsStrict("abc","ba")),
		?_assertEqual(false,containsStrict("","c")),
		?_assertEqual(false,containsStrict("","bc")),
		
		?_assertEqual(false,containsStrict("aaaaabcbbb","ac")),
		?_assertEqual(false,containsStrict("aaaaabcbbb","aaaaaa")),
		?_assertEqual(true,containsStrict("aaaaabcbbb","aaaaa")),
		?_assertEqual(false,containsStrict("aaaaabcbbb","aaaaac")),
		?_assertEqual(false,containsStrict("aaaaabcbbb","bc")),
		?_assertEqual(false,containsStrict("aaaaabcbbb","ab")),
		?_assertEqual(false,containsStrict("aaaaabcbbb","bbb")),
		?_assertEqual(false,containsStrict("aaaaabcbbb","abcd")),
		
		?_assertEqual(true,contains("","")),
		?_assertEqual(true,contains("abc","")),
		?_assertEqual(true,contains("abc","a")),
		?_assertEqual(true,contains("abc","b")),
		?_assertEqual(true,contains("abc","c")),
		?_assertEqual(true,contains("abc","bc")),
		?_assertEqual(true,contains("abc","ab")),
		?_assertEqual(true,contains("abc","abc")),

		?_assertEqual(false,contains("abc","d")),
		?_assertEqual(false,contains("abc","ac")),
		?_assertEqual(false,contains("abc","ba")),
		?_assertEqual(false,contains("","c")),
		?_assertEqual(false,contains("","bc")),
		
		?_assertEqual(false,contains("aaaaabcbbb","ac")),
		?_assertEqual(false,contains("aaaaabcbbb","aaaaaa")),
		?_assertEqual(false,contains("aaaaabcbbb","aaaaac")),
		?_assertEqual(true,contains("aaaaabcbbb","bc")),
		?_assertEqual(true,contains("aaaaabcbbb","ab")),
		?_assertEqual(true,contains("aaaaabcbbb","bbb")),
		?_assertEqual(false,contains("aaaaabcbbb","abcd"))
	].
	
updateconfiguration_test_() ->
	{"tests configuration updates",
	{inparallel,
	[
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,echo},receive {Ref,workerok} ->ok end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,updateConfiguration,[]},receive {Ref,ok} ->ok end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,updateConfiguration,[{}]},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"not a pair")) end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,updateConfiguration,[{"TT"}]},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"not a pair")) end end) end,

		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,updateConfiguration,[{'prefixClosed','true'}]},receive {Ref,ok} -> ok end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,updateConfiguration,[{'prefixClosed','TRUE'}]},receive {Ref,ok} -> ok end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,updateConfiguration,[{'prefixClosed','45'}]},receive {Ref,ok} -> ok end end) end, %% Java interprets everything that does not look like "true" as false.

		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,updateConfiguration,[{'gdKeyPairThreshold','0.45'}]},receive {Ref,ok} -> ok end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,updateConfiguration,[{'gdKeyPairThreshold','23'}]},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"failed to load value")) end end) end, %% value out of range
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,updateConfiguration,[{'gdKeyPairThreshold','-0.45'}]},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"failed to load value")) end end) end, %% value out of range
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,updateConfiguration,[{'gdKeyPairThreshold','45e-4'}]},receive {Ref,ok} -> ok end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,updateConfiguration,[{'gdKeyPairThreshold','45.67e-2'}]},receive {Ref,ok} -> ok end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,updateConfiguration,[{'gdKeyPairThreshold','tt'}]},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"failed to load value")) end end) end,


		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,updateConfiguration,[{'questionPathUnionLimit','5'}]},receive {Ref,ok} -> ok end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,updateConfiguration,[{'questionPathUnionLimit','-2'}]},receive {Ref,ok} -> ok end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,updateConfiguration,[{'questionPathUnionLimit','2.3'}]},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"failed to load value")) end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,updateConfiguration,[{'questionPathUnionLimit','2e-6'}]},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"failed to load value")) end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,updateConfiguration,[{'questionPathUnionLimit','tt'}]},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"failed to load value")) end end) end,

		
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,updateConfiguration,[{'learnerScoreMode','CONVENTIONAL'}]},receive {Ref,ok} -> ok end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,updateConfiguration,[{'learnerScoreMode','JUNK'}]},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"No enum constant")) end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,updateConfiguration,[{'learnerScoreMode',"nonsense"}]},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"OtpErlangString cannot be cast")) end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,updateConfiguration,[{'a','b'}]},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"unknown field a")) end end) end		
		
		%%?USEWORKER((Pid!{Ref,updateConfiguration,[]},receive {Ref,ok} ->ok end)) end
	]}}.

uploadtraces_test_() ->
	{"tests trace parsing",
	{inparallel,
	[
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,traces,a},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"Atom cannot be cast")) end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[aaa]},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"Atom cannot be cast to com.ericsson.otp.erlang.OtpErlangTuple")) end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[{pp,gg,eer}]},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"more than a pair of pos/neg")) end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[{[],gg}]},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"List cannot be cast to com.ericsson.otp.erlang.OtpErlangAtom")) end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[{pp,gg}]},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"instead of pos/neg")) end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[{pos,gg}]},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"Atom cannot be cast to com.ericsson.otp.erlang.OtpErlangList")) end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[{pos,["gg"]}]},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"String cannot be cast to com.ericsson.otp.erlang.OtpErlangAtom")) end end) end,

		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[ {pos,[a]}, aaa] },receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"Atom cannot be cast to com.ericsson.otp.erlang.OtpErlangTuple")) end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[ {pos,[a]}, {pp,gg,eer}] },receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"more than a pair of pos/neg")) end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[ {pos,[a]}, {[],gg}]},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"List cannot be cast to com.ericsson.otp.erlang.OtpErlangAtom")) end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[ {pos,[a]}, {pp,gg}]},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"instead of pos/neg")) end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[ {pos,[a]}, {pos,gg}]},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"Atom cannot be cast to com.ericsson.otp.erlang.OtpErlangList")) end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[ {pos,[a]}, {pos,["gg"]}]},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"String cannot be cast to com.ericsson.otp.erlang.OtpErlangAtom")) end end) end,
		
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[{pos,[a,b,c]},{pos,[a,b,c]},{neg,[a]}]},receive {Ref,ok} -> Pid!{Ref,getTraces},receive {Ref,ok,Pos,Neg} ->?assertEqual("[[a, b, c]]",Pos),?assertEqual("[[a]]",Neg) end end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[{pos,[a,b,c]},{pos,[a,b,c]},{neg,[a,a,a,a,a,a,aaa]},{neg,[a]}]},receive {Ref,ok} -> Pid!{Ref,getTraces},receive {Ref,ok,Pos,Neg} ->?assertEqual("[[a, b, c]]",Pos),?assertEqual("[[a], [a, a, a, a, a, a, aaa]]",Neg) end end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[{pos,[]}]},receive {Ref,ok} -> Pid!{Ref,getTraces},receive {Ref,ok,Pos,Neg} ->?assertEqual("[[]]",Pos),?assertEqual("[]",Neg) end end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[{neg,[]},{pos,[]}]},receive {Ref,ok} -> Pid!{Ref,getTraces},receive {Ref,ok,Pos,Neg} ->?assertEqual("[[]]",Pos),?assertEqual("[[]]",Neg) end end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,traces,[]},receive {Ref,ok} -> Pid!{Ref,getTraces},receive {Ref,ok,Pos,Neg} ->?assertEqual("[]",Pos),?assertEqual("[]",Neg) end end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,getTraces},receive {Ref,ok,Pos,Neg} ->?assertEqual("[]",Pos),?assertEqual("[]",Neg) end end) end
	]}}.

loadStatemachine_test_() ->
		{"tests FSM parsing",
	{inparallel,
	[
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,{
					  [a,b,c]
					  ,[{a,wibble,b},{b,wobble,c}]
					  ,a
					  ,[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"expected 5 components in FSM")) end end) end,

		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,{"a",
					  [a,b,c]
					  ,[{a,wibble,b},{b,wobble,c}]
					  ,a
					  ,[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"String cannot be cast to com.ericsson.otp.erlang.OtpErlangAtom")) end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,{b,
					  [a,b,c]
					  ,[{a,wibble,b},{b,wobble,c}]
					  ,a
					  ,[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"first element of a record should be")) end end) end,

		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=["a",b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"String cannot be cast to com.ericsson.otp.erlang.OtpErlangAtom")) end end) end,


		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,"c"]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"String cannot be cast to com.ericsson.otp.erlang.OtpErlangAtom")) end end) end,

		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,"wobble"]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"String cannot be cast to com.ericsson.otp.erlang.OtpErlangAtom")) end end) end,

		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=['',b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"empty state name")) end end) end,

		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=['',wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"empty label")) end end) end,


		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[d]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"invalid source state")) end end) end,

		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{'',wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"invalid source state")) end end) end,

		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{"a",wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"String cannot be cast to com.ericsson.otp.erlang.OtpErlangAtom")) end end) end,

		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibbleA,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"unknown label")) end end) end,
					 
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobbleA,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"unknown label")) end end) end,

		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,'',c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"unknown label")) end end) end,

		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,"wobble",c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"String cannot be cast to com.ericsson.otp.erlang.OtpErlangAtom")) end end) end,

		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},aa]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"Atom cannot be cast to com.ericsson.otp.erlang.OtpErlangTuple")) end end) end,

		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{a,wobble,c,d}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"expected 3 components in transition")) end end) end,

		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,bB},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"invalid target state")) end end) end,
					 
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,''},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"invalid target state")) end end) end,

		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,"b"},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"String cannot be cast to com.ericsson.otp.erlang.OtpErlangAtom")) end end) end,

		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[]
					  ,transitions=[]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"empty automaton")) end end) end,

		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state="a"
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"String cannot be cast to com.ericsson.otp.erlang.OtpErlangAtom")) end end) end,

		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=''
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"missing initial state")) end end) end,

		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=d
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"missing initial state")) end end) end,

		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{a,wibble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"non-determinism detected")) end end) end,

		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
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

		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,loadFSM,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,ok} -> Pid!{Ref,getFSM},receive {Ref,ok,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wobble,wibble]}} -> ok end end end) end
	]}}.
					  
loadStatemachineRelaxed_test_() ->
		{"tests relaxed FSM parsing",
	{inparallel,
	[
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,testLoadFSM,#statemachine{
					  states=[]
					  ,transitions=[{a,wibble,b},{'',wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"empty source state")) end end) end,
					 
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,testLoadFSM,#statemachine{
					  states=[]
					  ,transitions=[{a,wibble,b},{b,wobble,''}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"empty target state")) end end) end,
					 
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,testLoadFSM,#statemachine{
					  states=[]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=''
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"empty initial state")) end end) end,
					 
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,testLoadFSM,#statemachine{
					  states=[]
					  ,transitions=[{a,wibble,b},{b,'',c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"empty label")) end end) end,
					 
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,testLoadFSM,#statemachine{
					  states=[]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,ok,#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wobble,wibble]}} -> ok end end) end,					  
					  
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,testLoadFSM,#statemachine{
					  states=[d]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,ok,#statemachine{
					  states=[d,a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wobble,wibble]}} -> ok end end) end,

		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,testLoadFSM,#statemachine{
					  states=[d]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[]
					 }},receive {Ref,ok,#statemachine{
					  states=[d,a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wobble,wibble]}} -> ok end end) end,
					  
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,testLoadFSM,#statemachine{
					  states=[d]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[junk]
					 }},receive {Ref,ok,#statemachine{
					  states=[d,a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wobble,wibble]}} -> ok end end) end,
					  
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,testLoadFSM,#statemachine{
					  states=[d]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=e
					  ,alphabet=[wibble,wobble]
					 }},receive {Ref,ok,#statemachine{
					  states=[d,a,b,c,e]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=e
					  ,alphabet=[wobble,wibble]}} -> ok end end) end
					  					  
		%% The rest is tested with TestSynapse.testParseAutomata		  
			
	]}}.

parseMap_test_() ->
	{"tests map parsing",
	{inparallel,
	[
			fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,testMapParsing,a},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"Atom cannot be cast to com.ericsson.otp.erlang.OtpErlangList")) end end) end,
			fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,testMapParsing,[{a,b},a]},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"Atom cannot be cast to com.ericsson.otp.erlang.OtpErlangTuple")) end end) end,
			fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,testMapParsing,[{a,b},{a,''}]},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"empty second state name")) end end) end,
			fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,testMapParsing,[{a,b},{'',d}]},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"empty first state name")) end end) end,
			fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,testMapParsing,[{a,b},{"b",a}]},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"String cannot be cast to com.ericsson.otp.erlang.OtpErlangAtom")) end end) end,
			fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,testMapParsing,[{a,b},{b,"a"}]},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"String cannot be cast to com.ericsson.otp.erlang.OtpErlangAtom")) end end) end,

			fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,testMapParsing,[{a,b},{b,a}]},receive {Ref,ok,[{a,b},{b,a}]} -> ok end end) end,
			fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,testMapParsing,[{a,b},{a,a}]},receive {Ref,ok,[{a,a}]} -> ok end end) end,%% second pair overwrites the first one
			fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,testMapParsing,[{a,b}]},receive {Ref,ok,[{a,b}]} -> ok end end) end,
			fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,testMapParsing,[]},receive {Ref,ok,[]} -> ok end end) end
	]}}.
	
	