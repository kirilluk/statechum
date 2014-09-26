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

-module(test_fsmlearn).

-include_lib("eunit/include/eunit.hrl").
-include("synapse.hrl").
	
learn_test_()->
	{"tests learning",
	{inparallel,
	[
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> 
				Pid!{Ref,updateConfiguration,[{'askQuestions','false'},{'gdFailOnDuplicateNames','false'}]},receive {Ref,ok} -> %% no questions 
				Pid!{Ref,traces,[{neg,[a,b]},{pos,[a,a,a,b]}]},receive {Ref,ok} -> %% traces transferred	
				Pid!{Ref,learnEDSM},
				receive {Ref,ok,Fsm} -> % got the outcome, now check it for correctness
				Pid!{Ref,computeDiff, 
					Fsm,
					 #statemachine{
					  states=[s0,s1,s2,'N99']
					  ,transitions=[{s0,a,s1},{s1,a,s2},{s2,a,s2},{s2,b,s2},{s1,b,'N99'}]
					  ,initial_state=s0
					  ,alphabet=[a,b]
					 }
			},receive {Ref,ok,Difference} -> 
				Difference = #statemachinedifference{%% there has to be no differences
				added_transitions=[],
				deleted_transitions=[],
				added_states=[],
				deleted_states=[],
				name_mapping=[{'N1000','N99'},{'P1000',s0},{'P1001',s1},{'P1002',s2}],
				initial_state='P1000'},
				ok  end end end end end) end,

			% Learn Markov
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> 
				Pid!{Ref,updateConfiguration,[{'askQuestions','false'},{'gdFailOnDuplicateNames','false'}]},receive {Ref,ok} -> %% no questions 
				Pid!{Ref,traces,[{neg,[a,b]},{pos,[a,a,a,b]}]},receive {Ref,ok} -> %% traces transferred	
				Pid!{Ref,learn},
				receive {Ref,ok,Fsm} -> % got the outcome, now check it for correctness
				Pid!{Ref,computeDiff, 
					Fsm,
					 #statemachine{
					  states=[s0,s1,s2,s4,'N99']
					  ,transitions=[{s0,a,s1},{s1,a,s2},{s2,a,s2},{s1,b,'N99'},{s2,b,s4}]
					  ,initial_state=s0
					  ,alphabet=[a,b]
					 }
			},
				receive {Ref,ok,Difference} -> 
				Difference = #statemachinedifference{%% there has to be no differences
				added_transitions=[],
				deleted_transitions=[],
				added_states=[],
				deleted_states=[],
				name_mapping=[{'N1000','N99'},{'P1000',s0},{'P1001',s1},{'P1002',s2},{'P1004',s4}],
				initial_state='P1000'},
				ok  end end end end end) end,

			fun() -> testsynapse:useworker(fun(Pid,Ref) -> 
				Pid!{Ref,updateConfiguration,[{'askQuestions','false'},{'gdFailOnDuplicateNames','false'}]},receive {Ref,ok} -> %% no questions 
				Pid!{Ref,traces,[{neg,[a,b]},{pos,[a,a,a,b]}]},receive {Ref,ok} -> %% traces transferred	
				Pid!{Ref,learnEDSM},
				Pid!{Ref,stop},%% attempt to terminate
				receive {Ref,terminate} -> ok  end, %% make sure we got the response
				Pid!{Ref,updateConfiguration,[{'askQuestions','false'},{'gdFailOnDuplicateNames','false'}]},receive {Ref,ok} -> ok end, %% check the worker is alive
				Pid!{Ref,stop},receive {Ref,workerok} -> ok end, %% stop makes no difference to the worker 
				Pid!{Ref,stop},receive {Ref,workerok} -> ok end %% stop makes no difference to the worker 
				end end end) end,
				
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> 
				Pid!{Ref,updateConfiguration,[{'askQuestions','false'},{'gdFailOnDuplicateNames','false'}]},receive {Ref,ok} -> %% no questions 
				Pid!{Ref,traces,[{neg,[a,b]},{pos,[a,a,a,b]}]},receive {Ref,ok} -> %% traces transferred	
				Pid!{Ref,learnEDSM},
				Pid!{Ref,junk},%% attempt to terminate with an invalid command
				receive {Ref,ok,Fsm} -> ok  end, %% got the usual response
				Pid!{Ref,updateConfiguration,[{'askQuestions','false'},{'gdFailOnDuplicateNames','false'}]},receive {Ref,ok} -> ok end, %% check the worker is alive
				Pid!{Ref,stop},receive {Ref,workerok} -> ok end, %% stop makes no difference to the worker 
				Pid!{Ref,stop},receive {Ref,workerok} -> ok end %% stop makes no difference to the worker 
				end end end) end,
				
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> 
				Pid!{Ref,updateConfiguration,[{'askQuestions','false'},{'gdFailOnDuplicateNames','false'}]},receive {Ref,ok} -> %% no questions 
				Pid!{Ref,traces,[{neg,[a,b]},{pos,[a,a,a,b]}]},receive {Ref,ok} -> %% traces transferred	
				NotificationReceiver=spawn_link(testsynapse,handleNotifications,[Ref,0]),
				Pid!{Ref,learnEDSM, NotificationReceiver},
				receive {Ref,ok,Fsm} -> % an earlier test validated this
				NotificationReceiver!{Ref,self(),check},receive {Ref,3} ->ok end end %% got a few, in this case 3 
				end end end ) end,
				
				
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> 
				Pid!{Ref,updateConfiguration,[{'askQuestions','false'},{'gdFailOnDuplicateNames','false'}]},receive {Ref,ok} -> %% no questions 
				Pid!{Ref,traces,[{neg,[a,b]},{pos,[a,a,a,b]}]},receive {Ref,ok} -> %% traces transferred	
				NotificationReceiver=spawn_link(testsynapse,handleNotificationsAndRecordThem,[Ref,"learning"]),
				Pid!{Ref,learnEDSM,NotificationReceiver},
				receive {Ref,ok,Fsm} -> % an earlier test validated this
				NotificationReceiver!{Ref,self(),check},receive {Ref,V} -> ?assertEqual("learning 6 5 4",V) end end
				end end end ) end ,
				
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> 
				Pid!{Ref,updateConfiguration,[{'askQuestions','false'},{'gdFailOnDuplicateNames','false'},{'synapseSendFSMFrequency','2'}]},receive {Ref,ok} -> %% no questions 
				Pid!{Ref,traces,[{neg,[a,b]},{pos,[a,a,a,b]}]},receive {Ref,ok} -> %% traces transferred	
				NotificationReceiver=spawn_link(testsynapse,handleNotificationsAndRecordThem,[Ref,"learning"]),
				Pid!{Ref,learnEDSM,NotificationReceiver},
				receive {Ref,ok,Fsm} -> % an earlier test validated this
				NotificationReceiver!{Ref,self(),check},receive {Ref,V} -> ?assertEqual("learning 6 5<{statemachine,['P1000','P1001','P1002','N1000','P1004'],[{'P1000',a,'P1001'},{'P1001',a,'P1002'},{'P1001',b,'N1000'},{'P1002',a,'P1002'},{'P1002',b,'P1004'}],'P1000',[a,b]}><reds:['P1000','P1001','P1002','N1000']> 4",V) end end
				end end end ) end ,

			fun() -> testsynapse:useworker(fun(Pid,Ref) -> 
				Pid!{Ref,updateConfiguration,[{'askQuestions','false'},{'gdFailOnDuplicateNames','false'}]},receive {Ref,ok} -> %% no questions 
				Pid!{Ref,traces,[{neg,[a,b]},{pos,[a,a,a,b]}]},receive {Ref,ok} -> %% traces transferred	
				NotificationReceiver=spawn_link(testsynapse,handleNotifications,[Ref,0]),
				Pid!{Ref,learnEDSM},
				receive {Ref,ok,Fsm} -> % an earlier test validated this
				NotificationReceiver!{Ref,self(),check},receive {Ref,0} -> ok end end % here we did not tell the learner to notify, hence got zero 
				end end end ) end 
	]}}.
	
displayFSM_test_()->
	{"tests FSM visualisation",
	{inorder,
	[
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,displayFSM,
					#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 },
			"my graph"
			},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"String cannot be cast to com.ericsson.otp.erlang.OtpErlangAtom")),ok end end) end
			
%% This test actually pops a graph
%%			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,displayFSM,
%%					#statemachine{ states=[a,b,c],transitions=[{a,wibble,b},{b,wobble,c}],initial_state=a,alphabet=[wibble,wobble] },
%%			'my graph'},receive {Ref,ok} -> ok end end) end,

	]}}.