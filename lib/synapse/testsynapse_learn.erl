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

-module(testsynapse_learn).

-include_lib("eunit/include/eunit.hrl").
-include("synapse.hrl").

useworker(Function) ->
	StatechumRef=make_ref(),synapselauncher:find_statechum()!{self(),StatechumRef,getStatechumWorker},receive {StatechumRef,Pid} -> Ref = make_ref(),Function(Pid,Ref), Pid!{Ref,terminate} end.

notificationVisualiser(PidViz,RefViz,PidHost,RefHost, Counter) ->
	receive  
		{RefHost,status,step,{S,Fsm,Reds}} -> io:format(user,"[~w] Displaying FSM for ~w states ~n",[Counter,S]),
			useworker(fun(Pid,Ref) -> Pid!{Ref,updateConfiguration,[{'askQuestions','false'}]},receive {Ref,ok} ->ok end, %% no questions
				Pid!{Ref,loadFSM,Fsm},receive {Ref,ok} -> ok end, %% transferred FSM we got
				Pid!{Ref,setReds,Reds},receive {Ref,ok} -> ok end, %% assign red states
				Pid!{Ref,learn},receive {Ref,ok,Passive} -> ok end, %% learnt FSM passively
				#statemachine{states=ST,transitions=TR,alphabet=AL,initial_state=Init}=Passive,
				io:format(user,"Passive learnt: ~w states and ~w transitions ~n",[length(ST),length(TR)]), 
				PidViz!{RefViz,displayFSM,Passive,%%
				%%list_to_atom(lists:flatten(io_lib:format("Fsm_~w_~w",[Counter,S])))
				[]
				}, 
				receive {RefViz,ok} -> ok end,
				ok end),
			notificationVisualiser(PidViz,RefViz,PidHost,RefHost,Counter+1);
		{RefHost,status,step,{S}} -> io:format(user,"[~w] States so far: ~w~n",[Counter,S]),notificationVisualiser(PidViz,RefViz,PidHost,RefHost,Counter+1);
		{RefHost,check} -> PidHost!{RefHost, Counter};
		A -> throw("unexpected message " ++ lists:flatten(io_lib:format("~w",[A])) )
	end.


learnErlang_test_()->
	{"tests Erlang learning",
	{inorder, %% if run in parallel, I may end up attempting to start multiple runners at the same moment that will fail. 
	[
					 
	{timeout, 15000,
			fun() -> useworker(fun(Pid,Ref) -> 
				PidHost=self(),
				Pid!{Ref,updateConfiguration,[{'askQuestions','true'},{'gdFailOnDuplicateNames','false'},{'erlangInitialTraceLength','3'},{'erlangAlphabetAnyElements','ANY_WIBBLE'},{'synapseSendFSMFrequency','8'}]},receive {Ref,ok} ->ok end, %% no questions
				NotificationReceiver=spawn_link(fun() -> useworker(fun(PidViz,RefViz) ->notificationVisualiser(PidViz,RefViz,PidHost,Ref,0) end) end), 
				Pid!{Ref,learnErlang,'ErlangExamples/locker/locker.erl',NotificationReceiver},
				receive {Ref,ok,Fsm} -> 
					#statemachine{states=ST,transitions=TR,alphabet=AL,initial_state=Init}=Fsm,
					NotificationReceiver!{Ref,check},receive {Ref,Counter} -> ?assertEqual(41,Counter) end, %% wait for the visualiser to finish
					io:format(user,"Completed learning : ~w states and ~w transitions ~n",[length(ST),length(TR)]), 
					Pid!{Ref,displayFSM,Fsm,[],['N1000']}, %% now display the outcome
					receive {Ref,ok} -> ok end,
					ok end
				
				 end) end},
	{timeout, 15000,
			fun() -> useworker(fun(Pid,Ref) -> 
				PidHost=self(),
				Pid!{Ref,purgeModuleInformation},receive {Ref,ok} -> ok end,
				Pid!{Ref,updateConfiguration,[{'askQuestions','true'},{'gdFailOnDuplicateNames','false'},{'erlangInitialTraceLength','3'},{'erlangStripModuleNamesFromFunctionsInNonGenModules','false'},{'erlangAlphabetAnyElements','ANY_WIBBLE'},{'synapseSendFSMFrequency','-1'}]},receive {Ref,ok} ->ok end, %% no questions
				NotificationReceiver=spawn_link(fun() -> useworker(fun(PidViz,RefViz) ->notificationVisualiser(PidViz,RefViz,PidHost,Ref,0) end) end),

				Pid!{Ref,extractTypeInformation,'ErlangExamples/frequency/frequencyBroken.erl'},receive {Ref,ok,Types} -> TypeDict=dict:from_list(Types),
				FunToReplace='frequencyBroken:start/0',
				{Path,Line,FunName,FunArity,_Type}=dict:fetch(FunToReplace,TypeDict),
				Pid!{Ref,addTypeInformation,[{FunToReplace,{Path,Line,FunName,FunArity,{'Func',[],[],{'Any',[]}} }}]},receive {Ref,ok,Text} -> ok end,

				Pid!{Ref,learnErlang,'ErlangExamples/frequency/frequencyBroken.erl',NotificationReceiver},
				receive {Ref,ok,Fsm} -> 
					#statemachine{states=ST,transitions=TR,alphabet=AL,initial_state=Init}=Fsm,
					NotificationReceiver!{Ref,check},receive {Ref,Counter} -> ?assertEqual(54,Counter) end, %% wait for the visualiser to finish
					?assertEqual(5,length(ST)),?assertEqual(42,length(TR)),
					Pid!{Ref,displayFSM,Fsm,[],['N1000']}, %% now display the outcome
					receive {Ref,ok} -> ok end,
					ok end
				
				 end end) end},
	{timeout, 15000, %% Important: between the previous and the current test Synapse is not restarted, hence the changes we forced on the alphabet of frequencyBroken do not get purged and loading the same frequency again gets us the tweaked version of the module.
			fun() -> useworker(fun(Pid,Ref) -> 
				PidHost=self(),
				Pid!{Ref,purgeModuleInformation},receive {Ref,ok} -> ok end,
				Pid!{Ref,updateConfiguration,[{'askQuestions','true'},{'gdFailOnDuplicateNames','false'},{'erlangInitialTraceLength','3'},{'erlangStripModuleNamesFromFunctionsInNonGenModules','true'},{'erlangAlphabetAnyElements','ANY_WIBBLE'},{'synapseSendFSMFrequency','-1'}]},receive {Ref,ok} ->ok end, %% no questions
				NotificationReceiver=spawn_link(fun() -> useworker(fun(PidViz,RefViz) ->notificationVisualiser(PidViz,RefViz,PidHost,Ref,0) end) end), 

				Pid!{Ref,extractTypeInformation,'ErlangExamples/frequency/frequencyBroken.erl'},receive {Ref,ok,Types} -> TypeDict=dict:from_list(Types),
				FunToReplace='frequencyBroken:start/0',
				{Path,Line,FunName,FunArity,_Type}=dict:fetch(FunToReplace,TypeDict),
				Pid!{Ref,addTypeInformation,[{FunToReplace,{Path,Line,FunName,FunArity,{'Func',[],[],{'Any',[]}} }}]},receive {Ref,ok,Text} -> ok end,

				Pid!{Ref,learnErlang,'ErlangExamples/frequency/frequencyBroken.erl',NotificationReceiver},
				receive {Ref,ok,Fsm} -> 
					#statemachine{states=ST,transitions=TR,alphabet=AL,initial_state=Init}=Fsm,
					NotificationReceiver!{Ref,check},receive {Ref,Counter} -> ?assertEqual(54,Counter) end, %% wait for the visualiser to finish
					?assertEqual(5,length(ST)),?assertEqual(42,length(TR)),
					Pid!{Ref,displayFSM,Fsm,[],['N1000']}, %% now display the outcome
					receive {Ref,ok} -> ok end,
					ok end
				
				 end end) end},
	{timeout, 15000,
			fun() -> useworker(fun(Pid,Ref) -> 
				Pid!{Ref,updateConfiguration,[{'askQuestions','true'},{'gdFailOnDuplicateNames','false'},{'erlangInitialTraceLength','5'},{'erlangAlphabetAnyElements','ANY_WIBBLE'}]},receive {Ref,ok} -> %% no questions
				NotificationReceiver=spawn_link(testsynapse,handleNotifications,[Ref,0]), 
				Pid!{Ref,learnErlang,'ErlangExamples/locker/locker.erl',NotificationReceiver},
				receive {Ref,ok,#statemachine{states=S,transitions=TR,alphabet=AL,initial_state='P1000'}} -> % got the outcome, now lazily check it for correctness
				?assertEqual(6,length(S)),?assertEqual(11,length(AL)),?assertEqual(51,length(TR)),
				NotificationReceiver!{Ref,self(),check},receive {Ref,53} ->ok end
%% From TestErlangOracleLearner's testLockerLearning method: 
%%		Assert.assertEquals(6,locker.getStateNumber());
%%		Assert.assertEquals(11,locker.pathroutines.computeAlphabet().size());
%%		Assert.assertEquals(51,locker.pathroutines.countEdges());

				 end end end) end},

	{timeout, 15000,				 
		fun() -> useworker(fun(Pid,Ref) -> 
				Pid!{Ref,updateConfiguration,[{'askQuestions','false'},{'gdFailOnDuplicateNames','false'},{'erlangAlphabetAnyElements','ANY_WIBBLE'}]},receive {Ref,ok} -> %% no questions
				NotificationReceiver=spawn_link(testsynapse,handleNotifications,[Ref,0]), 
				Pid!{Ref,learnErlang,'ErlangExamples/locker/locker.erl',NotificationReceiver},
				receive {Ref,ok,Fsm} ->  % got the outcome, now check it for correctness
				Fsm=#statemachine{states=['P1000'],transitions=[],alphabet=[],initial_state='P1000'},
				NotificationReceiver!{Ref,self(),check},receive {Ref,1} ->ok end
				 end end end) end},
	{timeout, 15000,				 
		fun() -> useworker(fun(Pid,Ref) -> 
				Pid!{Ref,updateConfiguration,[{'askQuestions','false'},{'gdFailOnDuplicateNames','false'},{'erlangAlphabetAnyElements','ANY_WIBBLE'}]},receive {Ref,ok} -> %% no questions
				NotificationReceiver=spawn_link(testsynapse,handleNotifications,[Ref,0]), 
				Pid!{Ref,learnErlang,'ErlangExamples/locker/locker.erl'},
				receive {Ref,ok,Fsm} ->  % got the outcome, now check it for correctness
				Fsm=#statemachine{states=['P1000'],transitions=[],alphabet=[],initial_state='P1000'},
				NotificationReceiver!{Ref,self(),check},receive {Ref,0} ->ok end
				 end end end) end}
				 

	]}}.