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

-module(test_diff).

-include_lib("eunit/include/eunit.hrl").
-include("synapse.hrl").

parseMap_test_() ->
	{"tests map parsing",
	{inparallel,
	[
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,testMapParsing,a},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:cannotBeCast(Text,"Atom","com.ericsson.otp.erlang.OtpErlangList")) end end) end,
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,testMapParsing,[{a,b},a]},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:cannotBeCast(Text,"Atom","com.ericsson.otp.erlang.OtpErlangTuple")) end end) end,
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,testMapParsing,[{a,b},{a,''}]},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"empty second state name")) end end) end,
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,testMapParsing,[{a,b},{'',d}]},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"empty first state name")) end end) end,
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,testMapParsing,[{a,b},{"b",a}]},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:cannotBeCast(Text,"String","com.ericsson.otp.erlang.OtpErlangAtom")) end end) end,
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,testMapParsing,[{a,b},{b,"a"}]},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:cannotBeCast(Text,"String","com.ericsson.otp.erlang.OtpErlangAtom")) end end) end,

			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,testMapParsing,[{a,b},{b,a}]},receive {Ref,ok,[{a,b},{b,a}]} -> ok end end) end,
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,testMapParsing,[{a,b},{a,a}]},receive {Ref,ok,[{a,a}]} -> ok end end) end,%% second pair overwrites the first one
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,testMapParsing,[{a,b}]},receive {Ref,ok,[{a,b}]} -> ok end end) end,
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,testMapParsing,[]},receive {Ref,ok,[]} -> ok end end) end
	]}}.
	
computeDiff_test_()->
	{"tests diff computation",
	{inparallel,
	[
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,computeDiff,
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
				
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,computeDiff,
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
				
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,computeDiff,
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
				added_states=[c,d],
				deleted_states=[],
				name_mapping=[],
				initial_state=a}}->ok end end) end,

		%% a more elaborate case of renaming
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> 
				Pid!{Ref,computeDiff,
					#statemachine{
					  states=[a,b,c,d]
					  ,transitions=[{a,wibble,b},{b,wobble,c},{c,newone,d}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble,newone]
					 },
					 #statemachine{
					  states=[a,b,c,d]
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
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> 
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
					  states=[a,b,c,d,e]
					  ,transitions=[{a,wibble,b},{b,wobble,d},{d,newone,e},{e,wibble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble,newone]
					 }
			},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"are shared between A and B")),ok end end end) end,

			%% test for unknown label in the first machine
				fun() -> testsynapse:useworker(fun(Pid,Ref) -> 
				Pid!{Ref,computeDiff,
					#statemachine{
					  states=[a,b,c,d]
					  ,transitions=[{a,wibble,b},{b,wobble,c},{c,newone,d}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 },
					 #statemachine{
					  states=[a,b,c,d]
					  ,transitions=[{a,wibble,b},{b,wobble,d},{d,newone,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble,newone]
					 }
			},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"unknown label")),ok end end) end,
			
			%% test for unknown label in the second machine
				fun() -> testsynapse:useworker(fun(Pid,Ref) -> 
				Pid!{Ref,computeDiff,
					#statemachine{
					  states=[a,b,c,d]
					  ,transitions=[{a,wibble,b},{b,wobble,c},{c,newone,d}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble,newone]
					 },
					 #statemachine{
					  states=[a,b,c,d]
					  ,transitions=[{a,wibble,b},{b,wobble,d},{d,newone,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 }
			},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"unknown label")),ok end end) end
			
	]}}.

					
diffLoad_test_()->
	{"tests diff loading",
	{inorder,
	[
	
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,testDiffParsing,
					#statemachine{states=[a],transitions=[],initial_state=a,alphabet=[]}, %% dummy machine since this is testing error handling of the differences
			{statemachinedifference,a,
				[],
				[],
				[],
				[],
				[],
				a}
			},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"expected 7 components in diff")),ok end end) end,
	
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,testDiffParsing,
					#statemachine{states=[a],transitions=[],initial_state=a,alphabet=[]}, %% dummy machine since this is testing error handling of the differences
			{aa,
				[],
				[],
				[],
				[],
				[],
				a}
			},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"statemachinedifference")),ok end end) end,
	
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,testDiffParsing,
					#statemachine{states=[a],transitions=[],initial_state=a,alphabet=[]}, %% dummy machine since this is testing error handling of the differences
			#statemachinedifference{
				added_transitions=a,
				deleted_transitions=[],
				added_states=[],
				deleted_states=[],
				name_mapping=[],
				initial_state=a}
			},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:cannotBeCast(Text,"Atom","com.ericsson.otp.erlang.OtpErlangList")),ok end end) end,
	
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,testDiffParsing,
					#statemachine{states=[a],transitions=[],initial_state=a,alphabet=[]}, %% dummy machine since this is testing error handling of the differences
			#statemachinedifference{
				added_transitions=[],
				deleted_transitions=[],
				added_states=[],
				deleted_states=[],
				name_mapping=[],
				initial_state="a"}
			},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:cannotBeCast(Text,"String","com.ericsson.otp.erlang.OtpErlangAtom")),ok end end) end,
	
	%% invalid diff
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,testDiffParsing,
					#statemachine{states=[a],transitions=[],initial_state=a,alphabet=[]},
			#statemachinedifference{
				added_transitions=[],
				deleted_transitions=[{b,wobble,c}],
				added_states=[],
				deleted_states=[],
				name_mapping=[],
				initial_state=a}
			},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"source state in diff is not known")),ok end end) end,

	%% invalid diff
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,testDiffParsing,
					#statemachine{states=[a],transitions=[],initial_state=a,alphabet=[]},
			#statemachinedifference{
				added_transitions=[],
				deleted_transitions=[{a,wobble,c}],
				added_states=[],
				deleted_states=[],
				name_mapping=[],
				initial_state=a}
			},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"target state in diff is not known")),ok end end) end,

	%% invalid diff
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,testDiffParsing,
					#statemachine{states=[a],transitions=[],initial_state=a,alphabet=[]},
			#statemachinedifference{
				added_transitions=[],
				deleted_transitions=[{a,wobble,a}],
				added_states=[],
				deleted_states=[],
				name_mapping=[],
				initial_state=a}
			},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"edge in diff was not found")),ok end end) end,

			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,testDiffParsing,
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
			},receive {Ref,ok,'[a-[wibble]->b:null, b-[waggle]->d:java.awt.Color[r=0,g=255,b=0], b-[wobble]->c:java.awt.Color[r=255,g=0,b=0]]'} -> 
			ok % ?debugFmt("received: ~p~n",[AA]),timer:sleep(10000) % thanks to http://stackoverflow.com/questions/9233310/eunit-output-debug-info-from-tested-modules
			 end end) end,
				
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,testDiffParsing,
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
			},receive {Ref,ok,'[a-[wibble]->b:null, b-[wobble]->c:null]'} -> ok end end) end,
				
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,displayDiff,
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
			},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:cannotBeCast(Text,"String","com.ericsson.otp.erlang.OtpErlangAtom")),ok end end) end
				
%% This test actually pops a graph
%%			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,displayDiff,
%%					#statemachine{ states=[a,b,c],transitions=[{a,wibble,b},{b,wobble,c}],initial_state=a,alphabet=[wibble,wobble] },
%%			#statemachinedifference{added_transitions=[{b,waggle,d}],deleted_transitions=[{b,wobble,c}],added_states=[d],deleted_states=[],name_mapping=[],initial_state=a},
%%			'my graph'},receive {Ref,ok} -> ok end end) end

	]}}.
	
		
