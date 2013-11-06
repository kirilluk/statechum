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
	
computeDiff_test_()->
	{"tests diff computation",
	{inparallel,
	[
			fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,computeDiff,
					#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 },
					 #statemachine{
					  states=[a,b,d]
					  ,transitions=[{a,wibble,b},{b,waggle,d}]
					  ,initial_state=a
					  ,alphabet=[wibble,waggle]
					 }
			},receive {Ref,ok,
			#statemachinedifference{
				added_transitions=[{b,waggle,d}],
				deleted_transitions=[{b,wobble,c}],
				added_states=[d],
				deleted_states=[c],
				name_mapping=[],
				initial_state=a}} -> ok end end) end,
				
			fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,computeDiff,
					#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 },
					 #statemachine{
					  states=[p,q,d]
					  ,transitions=[{p,wibble,q},{q,waggle,d}] %% Here states carry a different name which is why a map will be non-empty
					  ,initial_state=p
					  ,alphabet=[wibble,waggle]
					 }
			},receive {Ref,ok,
			#statemachinedifference{
				added_transitions=[{b,waggle,d}],
				deleted_transitions=[{b,wobble,c}],
				added_states=[d],
				deleted_states=[c],
				name_mapping=[{a,p},{b,q}],
				initial_state=a}} -> ok end end) end,
				
			fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,computeDiff,
					#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 },
					 #statemachine{
					  states=[a,b,c,d] %% c is a disconnected state which is why it is preserved and hence not mentioned in removed states
					  ,transitions=[{a,wibble,b},{b,waggle,d}]
					  ,initial_state=a
					  ,alphabet=[wibble,waggle]
					 }
			},receive {Ref,ok,
			#statemachinedifference{
				added_transitions=[{b,waggle,d}],
				deleted_transitions=[{b,wobble,c}],
				added_states=[d],
				deleted_states=[],
				name_mapping=[],
				initial_state=a}} -> ok end end) end,

		%% a more elaborate case of renaming
			fun() -> useworker(fun(Pid,Ref) -> 
				Pid!{Ref,computeDiff,
					#statemachine{
					  states=[a,b,c,d]
					  ,transitions=[{a,wibble,b},{b,wobble,c},{c,newone,d}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble,newone]
					 },
					 #statemachine{
					  states=[a,b,c,d] %% c is a disconnected state which is why it is preserved and hence not mentioned in removed states
					  ,transitions=[{a,wibble,b},{b,wobble,d},{d,newone,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble,newone]
					 }
			},receive {Ref,ok,#statemachinedifference{
				added_transitions=[],
				deleted_transitions=[],
				added_states=[],
				deleted_states=[],
				name_mapping=[{c,d},{d,c}],
				initial_state=a}} -> ok end end) end,

%% rejection of duplicate states if prohibited by the configuration
			fun() -> useworker(fun(Pid,Ref) -> 
				Pid!{Ref,updateConfiguration,[{'gdFailOnDuplicateNames','true'}]},
				receive {Ref,ok} ->

				Pid!{Ref,computeDiff,
					#statemachine{
					  states=[a,b,c,d]
					  ,transitions=[{a,wibble,b},{b,wobble,c},{c,newone,d}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble,newone]
					 },
					 #statemachine{
					  states=[a,b,c,d,e] %% c is a disconnected state which is why it is preserved and hence not mentioned in removed states
					  ,transitions=[{a,wibble,b},{b,wobble,d},{d,newone,e},{e,wibble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble,newone]
					 }
			},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"are shared between A and B")),ok end end end) end,


			%% test for unknown label in the first machine
				fun() -> useworker(fun(Pid,Ref) -> 
				Pid!{Ref,computeDiff,
					#statemachine{
					  states=[a,b,c,d]
					  ,transitions=[{a,wibble,b},{b,wobble,c},{c,newone,d}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 },
					 #statemachine{
					  states=[a,b,c,d] %% c is a disconnected state which is why it is preserved and hence not mentioned in removed states
					  ,transitions=[{a,wibble,b},{b,wobble,d},{d,newone,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble,newone]
					 }
			},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"unknown label")),ok end end) end,
			
			%% test for unknown label in the second machine
				fun() -> useworker(fun(Pid,Ref) -> 
				Pid!{Ref,computeDiff,
					#statemachine{
					  states=[a,b,c,d]
					  ,transitions=[{a,wibble,b},{b,wobble,c},{c,newone,d}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble,newone]
					 },
					 #statemachine{
					  states=[a,b,c,d] %% c is a disconnected state which is why it is preserved and hence not mentioned in removed states
					  ,transitions=[{a,wibble,b},{b,wobble,d},{d,newone,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }
			},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"unknown label")),ok end end) end
	]}}.
						
diffLoad_test_()->
	{"tests diff loading",
	{inorder,
	[
	
			fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,testDiffParsing,
					#statemachine{states=[a],transitions=[],initial_state=a,alphabet=[]}, %% dummy machine since this is testing error handling of the differences
			{statemachinedifference,a,
				[],
				[],
				[],
				[],
				[],
				a}
			},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"expected 7 components in diff")),ok end end) end,
	
			fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,testDiffParsing,
					#statemachine{states=[a],transitions=[],initial_state=a,alphabet=[]}, %% dummy machine since this is testing error handling of the differences
			{aa,
				[],
				[],
				[],
				[],
				[],
				a}
			},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"statemachinedifference")),ok end end) end,
	
			fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,testDiffParsing,
					#statemachine{states=[a],transitions=[],initial_state=a,alphabet=[]}, %% dummy machine since this is testing error handling of the differences
			#statemachinedifference{
				added_transitions=a,
				deleted_transitions=[],
				added_states=[],
				deleted_states=[],
				name_mapping=[],
				initial_state=a}
			},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"Atom cannot be cast to com.ericsson.otp.erlang.OtpErlangList")),ok end end) end,
	
			fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,testDiffParsing,
					#statemachine{states=[a],transitions=[],initial_state=a,alphabet=[]}, %% dummy machine since this is testing error handling of the differences
			#statemachinedifference{
				added_transitions=[],
				deleted_transitions=[],
				added_states=[],
				deleted_states=[],
				name_mapping=[],
				initial_state="a"}
			},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"String cannot be cast to com.ericsson.otp.erlang.OtpErlangAtom")),ok end end) end,
	
	%% invalid diff
			fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,testDiffParsing,
					#statemachine{states=[a],transitions=[],initial_state=a,alphabet=[]},
			#statemachinedifference{
				added_transitions=[],
				deleted_transitions=[{b,wobble,c}],
				added_states=[],
				deleted_states=[],
				name_mapping=[],
				initial_state=a}
			},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"source state in diff is not known")),ok end end) end,

	%% invalid diff
			fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,testDiffParsing,
					#statemachine{states=[a],transitions=[],initial_state=a,alphabet=[]},
			#statemachinedifference{
				added_transitions=[],
				deleted_transitions=[{a,wobble,c}],
				added_states=[],
				deleted_states=[],
				name_mapping=[],
				initial_state=a}
			},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"target state in diff is not known")),ok end end) end,

	%% invalid diff
			fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,testDiffParsing,
					#statemachine{states=[a],transitions=[],initial_state=a,alphabet=[]},
			#statemachinedifference{
				added_transitions=[],
				deleted_transitions=[{a,wobble,a}],
				added_states=[],
				deleted_states=[],
				name_mapping=[],
				initial_state=a}
			},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"edge in diff was not found")),ok end end) end,

			fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,testDiffParsing,
					#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 },
			#statemachinedifference{
				added_transitions=[{b,waggle,d}],
				deleted_transitions=[{b,wobble,c}],
				added_states=[d],
				deleted_states=[],
				name_mapping=[],
				initial_state=a}
			},receive {Ref,ok,'a-[wibble]->b:null,b-[wobble]->c:java.awt.Color[r=255,g=0,b=0],b-[waggle]->d:java.awt.Color[r=0,g=255,b=0]'} -> ok end end) end,
				
			fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,testDiffParsing,
					#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 },
			#statemachinedifference{
				added_transitions=[],
				deleted_transitions=[],
				added_states=[],
				deleted_states=[],
				name_mapping=[],
				initial_state=a}
			},receive {Ref,ok,'a-[wibble]->b:null,b-[wobble]->c:null'} -> ok end end) end,
				
			fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,displayDiff,
					#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 },
			#statemachinedifference{
				added_transitions=[{b,waggle,d}],
				deleted_transitions=[{b,wobble,c}],
				added_states=[d],
				deleted_states=[],
				name_mapping=[],
				initial_state=a},
			"my graph"
			},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"String cannot be cast to com.ericsson.otp.erlang.OtpErlangAtom")),ok end end) end,
			
			fun() -> useworker(fun(Pid,Ref) -> ThisProcess = self(),NotificationReceiver=spawn(fun() -> receive {Ref,step} -> receive {Ref,check} -> ThisProcess!{Ref,received} end end end),
				 Pid!{Ref,displayDiff,
					#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 },
			#statemachinedifference{
				added_transitions=[{b,waggle,d}],
				deleted_transitions=[{b,wobble,c}],
				added_states=[d],
				deleted_states=[],
				name_mapping=[],
				initial_state=a},
			[],NotificationReceiver
			},receive {Ref,ok} -> NotificationReceiver!{Ref,check},receive {Ref,received} -> ok end end end) end,

			fun() -> useworker(fun(Pid,Ref) -> ThisProcess = self(),NotificationReceiver=spawn_link(fun() -> receive {Ref,step} -> throw('should_not_receive_this'); {Ref,check} -> ThisProcess!{Ref,complete} end end),
				 Pid!{Ref,displayDiff,
					#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 },
			#statemachinedifference{
				added_transitions=[{b,waggle,d}],
				deleted_transitions=[{b,wobble,c}],
				added_states=[d],
				deleted_states=[],
				name_mapping=[],
				initial_state=a},
			[]
			},receive {Ref,ok} -> NotificationReceiver!{Ref,check},receive {Ref,complete} -> ok end end end) end

	
%% This test actually pops a graph
%%			fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,displayDiff,
%%					#statemachine{ states=[a,b,c],transitions=[{a,wibble,b},{b,wobble,c}],initial_state=a,alphabet=[wibble,wobble] },
%%			#statemachinedifference{added_transitions=[{b,waggle,d}],deleted_transitions=[{b,wobble,c}],added_states=[d],deleted_states=[],name_mapping=[],initial_state=a},
%%			'my graph'},receive {Ref,ok} -> ok end end) end

	]}}.

	
learn_test_()->
	{"tests learning",
	{inparallel,
	[
			fun() -> useworker(fun(Pid,Ref) -> 
				Pid!{Ref,updateConfiguration,[{'askQuestions','false'},{'gdFailOnDuplicateNames','false'}]},receive {Ref,ok} -> %% no questions 
				Pid!{Ref,traces,[{neg,[a,b]},{pos,[a,a,a,b]}]},receive {Ref,ok} -> %% traces transferred	
				Pid!{Ref,learn},
				receive {Ref,ok,Fsm} -> % got the outcome, now check it for correctness
				Pid!{Ref,computeDiff, 
					Fsm,
					 #statemachine{
					  states=[s0,s1,s2,n]
					  ,transitions=[{s0,a,s1},{s1,a,s2},{s2,a,s2},{s2,b,s2},{s1,b,n}]
					  ,initial_state=s0
					  ,alphabet=[a,b]
					 }
			},receive {Ref,ok,
				#statemachinedifference{%% there has to be no differences
				added_transitions=[],
				deleted_transitions=[],
				added_states=[],
				deleted_states=[],
				name_mapping=[{'N1000',n},{'P1000',s0},{'P1001',s1},{'P1002',s2}],
				initial_state='P1000'}} ->
				ok end end end end end) end,

			fun() -> useworker(fun(Pid,Ref) -> 
				Pid!{Ref,updateConfiguration,[{'askQuestions','false'},{'gdFailOnDuplicateNames','false'}]},receive {Ref,ok} -> %% no questions 
				Pid!{Ref,traces,[{neg,[a,b]},{pos,[a,a,a,b]}]},receive {Ref,ok} -> %% traces transferred	
				Pid!{Ref,learn},
				Pid!{Ref,stop},%% attempt to terminate
				receive {Ref,terminate} -> ok  end, %% make sure we got the response
				Pid!{Ref,updateConfiguration,[{'askQuestions','false'},{'gdFailOnDuplicateNames','false'}]},receive {Ref,ok} -> ok end, %% check the worker is alive
				Pid!{Ref,stop},receive {Ref,workerok} -> ok end, %% stop makes no difference to the worker 
				Pid!{Ref,stop},receive {Ref,workerok} -> ok end %% stop makes no difference to the worker 
				end end end) end,
				
			fun() -> useworker(fun(Pid,Ref) -> 
				Pid!{Ref,updateConfiguration,[{'askQuestions','false'},{'gdFailOnDuplicateNames','false'}]},receive {Ref,ok} -> %% no questions 
				Pid!{Ref,traces,[{neg,[a,b]},{pos,[a,a,a,b]}]},receive {Ref,ok} -> %% traces transferred	
				Pid!{Ref,learn},
				Pid!{Ref,junk},%% attempt to terminate with an invalid command
				receive {Ref,ok,Fsm} -> ok  end, %% got the usual response
				Pid!{Ref,updateConfiguration,[{'askQuestions','false'},{'gdFailOnDuplicateNames','false'}]},receive {Ref,ok} -> ok end, %% check the worker is alive
				Pid!{Ref,stop},receive {Ref,workerok} -> ok end, %% stop makes no difference to the worker 
				Pid!{Ref,stop},receive {Ref,workerok} -> ok end %% stop makes no difference to the worker 
				end end end) end,
				
			fun() -> useworker(fun(Pid,Ref) -> 
				Pid!{Ref,updateConfiguration,[{'askQuestions','false'},{'gdFailOnDuplicateNames','false'}]},receive {Ref,ok} -> %% no questions 
				Pid!{Ref,traces,[{neg,[a,b]},{pos,[a,a,a,b]}]},receive {Ref,ok} -> %% traces transferred	
				NotificationReceiver=spawn_link(synapselauncher,handleNotifications,[Ref,0]),
				Pid!{Ref,learn, NotificationReceiver},
				receive {Ref,ok,Fsm} -> % an earlier test validated this
				NotificationReceiver!{Ref,self(),check},receive {Ref,3} ->ok end end %% got a few, in this case 3 
				end end end ) end,
				
			fun() -> useworker(fun(Pid,Ref) -> 
				Pid!{Ref,updateConfiguration,[{'askQuestions','false'},{'gdFailOnDuplicateNames','false'}]},receive {Ref,ok} -> %% no questions 
				Pid!{Ref,traces,[{neg,[a,b]},{pos,[a,a,a,b]}]},receive {Ref,ok} -> %% traces transferred	
				NotificationReceiver=spawn_link(synapselauncher,handleNotifications,[Ref,0]),
				Pid!{Ref,learn},
				receive {Ref,ok,Fsm} -> % an earlier test validated this
				NotificationReceiver!{Ref,self(),check},receive {Ref,0} -> ok end end % here we did not tell the learner to notify, hence got zero 
				end end end ) end 
				
	]}}.
	
displayFSM_test_()->
	{"tests FSM visualisation",
	{inorder,
	[
			fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,displayFSM,
					#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 },
			"my graph"
			},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"String cannot be cast to com.ericsson.otp.erlang.OtpErlangAtom")),ok end end) end,
			
			fun() -> useworker(fun(Pid,Ref) -> ThisProcess = self(),NotificationReceiver=spawn(fun() -> receive {Ref,step} -> receive {Ref,check} -> ThisProcess!{Ref,received} end end end),
				 Pid!{Ref,displayFSM,
					#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 },
			[],NotificationReceiver
			},receive {Ref,ok} -> NotificationReceiver!{Ref,check},receive {Ref,received} -> ok end end end) end,

%% This test actually pops a graph
%%			fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,displayFSM,
%%					#statemachine{ states=[a,b,c],transitions=[{a,wibble,b},{b,wobble,c}],initial_state=a,alphabet=[wibble,wobble] },
%%			'my graph'},receive {Ref,ok} -> ok end end) end,

			fun() -> useworker(fun(Pid,Ref) -> ThisProcess = self(),NotificationReceiver=spawn_link(fun() -> receive {Ref,step} -> throw('should_not_receive_this'); {Ref,check} -> ThisProcess!{Ref,complete} end end),
				 Pid!{Ref,displayFSM,
					#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 },
			[]
			},receive {Ref,ok} -> NotificationReceiver!{Ref,check},receive {Ref,complete} -> ok end end end) end
	]}}.
	