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

-module(test_parsetypemap).

-include_lib("eunit/include/eunit.hrl").
-include("synapse.hrl").

parseTypeMap_test_() ->
	{"tests type map parsing",
	{inparallel,
	[
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,addTypeInformation,a},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"Atom cannot be cast to com.ericsson.otp.erlang.OtpErlangList")) end end) end,
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,addTypeInformation,[a]},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"element of a list should be a tuple")) end end) end,
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,addTypeInformation,[{66}]},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"exactly two elements")) end end) end,
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,addTypeInformation,[{66,77}]},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"should be an atom")) end end) end,
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,addTypeInformation,[{'66',77}]},receive {Ref,failure,Text} -> ?assertEqual(true,testsynapse:contains(Text,"should be a tuple")) end end) end,
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,addTypeInformation,[{'a',{b}}]},receive {Ref,ok,Text} -> ?assertEqual([{'a',{'b'}}],Text) end end) end,
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,addTypeInformation,[{'a',{b}},{'b',{77}}]},receive {Ref,ok,Text} -> ?assertEqual([{'a',{'b'}},{'b',{77}}],Text) end end) end,
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,addTypeInformation,[{'a',{b}},{'b',{77}}]},receive {Ref,ok,Text} -> ?assertEqual([{'a',{'b'}},{'b',{77}}],Text) end, 
				Pid!{Ref,addTypeInformation,[{'a',{c}}]},receive {Ref,ok,AText} -> ?assertEqual([{'a',{'c'}},{'b',{77}}],AText) end end) end, %% tests override of an existing value
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,addTypeInformation,[{'a',{b}},{'a',{c}}]},receive {Ref,ok,Text} -> ?assertEqual([{'a',{'c'}}],Text) end end) end %% tests override of an existing value
	]}}.
	
parseTypeMapLoad_test_() ->
	{"tests type map loading",
	{inorder,
	[
			fun() -> testsynapse:useworker(fun(Pid,Ref) -> Pid!{Ref,extractTypeInformation,'ErlangExamples/locker/locker.erl'},receive {Ref,ok,Types} -> TypeDict=dict:from_list(Types),?assert(4 < dict:size(TypeDict)),
				{Path,
          10,handle_call,2,
          {'Func',[],
              [{'Alt',[],
                   [{'Atom',[],[lock,read,unlock]},
                    {'Tuple',[],[{'Atom',[],[write]},{'Any',[]}]}]},
               {'Tuple',[],[{'Any',[]},{'Any',[]}]}],
              {'Tuple',[],
                  [{'Atom',[],[reply]},
                   {'Any',[]},
                   {'Tuple',[],[{'Any',[]},{'Any',[]}]}]}}}=dict:fetch('locker:handle_call/2',TypeDict),
                  ?assertEqual(true,testsynapse:contains(Path,"locker.erl")),
                  
         {Path,
          25,handle_call,3,
          {'Func',[],
              [{'Alt',[],
                   [{'Atom',[],[lock,read,unlock]},
                    {'Tuple',[],[{'Atom',[],[write]},{'Any',[]}]}]},
               {'Any',[]},
               {'Tuple',[],[{'Any',[]},{'Any',[]}]}],
              {'Tuple',[],
                  [{'Atom',[],[reply]},
                   {'Any',[]},
                   {'Tuple',[],[{'Any',[]},{'Any',[]}]}]}}}=dict:fetch('locker:handle_call/3',TypeDict),
                   
		{Path,
	     28,handle_cast,2,
	     {'Func',[],
	         [{'Any',[]},{'Any',[]}],
	         {'Alt',[],
	             [{'Tuple',[],[{'Atom',[],[noreply]},{'Any',[]}]},
	              {'Tuple',[],
	                  [{'Atom',[],[stop]},
	                   {'Atom',[],[normal]},
	                   {'Atom',[],[stopped]}]}]}}}=dict:fetch('locker:handle_cast/2',TypeDict),
	                   
	                   
	    {Path,
	     7,init,1,
	     {'Func',[],
	         [{'Any',[]}],
	         {'Tuple',[],
	             [{'Atom',[],[ok]},
	              {'Tuple',[],[{'Atom',[],[unlocked]},{'Int',[values],[-1]}]}]}}}=dict:fetch('locker:init/1',TypeDict),

		{Path,
         33,terminate,2,
            {'Func',[],[{'Any',[]},{'Any',[]}],{'Atom',[],[ok]}}}=dict:fetch('locker:terminate/2',TypeDict)
	 %%,[A|[B|[C|[D|[E|[F|[G|Other]]]]]]]=Types,io:format(user,"GOT: ~p~n~p~n~p~n~p~n~p~n",[A,D,E,F,G])
			 end end) end,
			 
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> 
				Pid!{Ref,purgeModuleInformation},receive {Ref,ok} -> ok end,
		
				Pid!{Ref,extractTypeInformation,'ErlangExamples/frequency/frequencyBroken.erl'},receive {Ref,ok,Types} -> TypeDict=dict:from_list(Types),
				FunToReplace='frequencyBroken:start/0',
				{Path,Line,FunName,FunArity,_Type}=dict:fetch(FunToReplace,TypeDict),
%%				Pid!{Ref,addTypeInformation,[{FunToReplace,{Path,Line,FunName,FunArity,{'Func',[],[],{'Any',[]}} }}]},receive {Ref,failure,Text} -> io:format(user,"Failure received: ~p~n",[Text]) end
				Pid!{Ref,addTypeInformation,[{FunToReplace,{Path,Line,FunName,FunArity,{'FuncA',[],[],{'Any',[]}} }}]},receive {Ref,ok,TextType} -> ok end,
				Pid!{Ref,learnErlang,'ErlangExamples/frequency/frequencyBroken.erl'},
				receive {Ref,failure,Text} ->?assertEqual(true,testsynapse:contains(Text,"does not start with the 'Func' tag")) end 
				%%io:format(user,"Failure received: ~p~n",[Text]) end
		 end end) end,
			 
		fun() -> testsynapse:useworker(fun(Pid,Ref) -> 
				Pid!{Ref,purgeModuleInformation},receive {Ref,ok} -> ok end,
				
				Pid!{Ref,extractTypeInformation,'ErlangExamples/frequency/frequencyBroken.erl'},receive {Ref,ok,Types} -> TypeDict=dict:from_list(Types),
				FunToReplace='frequencyBroken:start/0',
				{Path,Line,FunName,FunArity,_Type}=dict:fetch(FunToReplace,TypeDict),
				Pid!{Ref,addTypeInformation,[{FunToReplace,{Path,Line }}]},receive {Ref,ok,TextType} -> ok end,
				Pid!{Ref,learnErlang,'ErlangExamples/frequency/frequencyBroken.erl'},
				receive {Ref,failure,Text} ->?assertEqual(true,testsynapse:contains(Text,"invalid type of an Erlang function")) end 
				%%io:format(user,"Failure received: ~p~n",[Text]) end
		 end end) end,

		fun() -> testsynapse:useworker(fun(Pid,Ref) -> 
				Pid!{Ref,purgeModuleInformation},receive {Ref,ok} -> ok end,
				
				Pid!{Ref,extractTypeInformation,'ErlangExamples/frequency/frequencyBroken.erl'},receive {Ref,ok,Types} -> TypeDict=dict:from_list(Types),
				FunToReplace='frequencyBroken:start/0',
				{Path,Line,FunName,FunArity,_Type}=dict:fetch(FunToReplace,TypeDict),
				Pid!{Ref,addTypeInformation,[{FunToReplace,{Path,Line,'differentName',FunArity,{'Func',[],[],{'Any',[]}} }}]},receive {Ref,ok,TextType} -> ok end,
				Pid!{Ref,learnErlang,'ErlangExamples/frequency/frequencyBroken.erl'},
				receive {Ref,failure,Text} ->?assertEqual(true,testsynapse:contains(Text,"invalid override")) end 
				%%io:format(user,"Failure received: ~p~n",[Text]) end
		 end end) end,

		fun() -> testsynapse:useworker(fun(Pid,Ref) -> 
				Pid!{Ref,purgeModuleInformation},receive {Ref,ok} -> ok end,
				
				Pid!{Ref,extractTypeInformation,'ErlangExamples/frequency/frequencyBroken.erl'},receive {Ref,ok,Types} -> TypeDict=dict:from_list(Types),
				FunToReplace='frequencyBroken:start/0',
				{Path,Line,FunName,FunArity,_Type}=dict:fetch(FunToReplace,TypeDict),
				Pid!{Ref,addTypeInformation,[{FunToReplace,{Path,Line,"differentName",FunArity,{'Func',[],[],{'Any',[]}} }}]},receive {Ref,ok,TextType} -> ok end,
				Pid!{Ref,learnErlang,'ErlangExamples/frequency/frequencyBroken.erl'},
				receive {Ref,failure,Text} ->?assertEqual(true,testsynapse:contains(Text,"com.ericsson.otp.erlang.OtpErlangString cannot be cast to com.ericsson.otp.erlang.OtpErlangAtom")) end 
				%%io:format(user,"Failure received: ~p~n",[Text]) end
		 end end) end,

		fun() -> testsynapse:useworker(fun(Pid,Ref) -> 
				Pid!{Ref,purgeModuleInformation},receive {Ref,ok} -> ok end,
		
				Pid!{Ref,extractTypeInformation,'ErlangExamples/frequency/frequencyBroken.erl'},receive {Ref,ok,Types} -> TypeDict=dict:from_list(Types),
				FunToReplace='frequencyBroken:start/0',
				{Path,Line,FunName,FunArity,_Type}=dict:fetch(FunToReplace,TypeDict),
				Pid!{Ref,addTypeInformation,[{FunToReplace,{"junk",Line,FunName,FunArity,{'Func',[],[],{'Any',[]}} }}]},receive {Ref,ok,TextType} -> ok end,
				Pid!{Ref,learnErlang,'ErlangExamples/frequency/frequencyBroken.erl'},
				receive {Ref,failure,Text} ->?assertEqual(true,testsynapse:contains(Text,"Invalid module junk")) end 
				%%io:format(user,"Failure received: ~p~n",[Text]) end
		 end end) end
	]}}.