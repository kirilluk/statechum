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

-export([handleNotifications/2,handleNotificationsAndRecordThem/2,useworker/1,containsStrict/2,contains/2,containsCast/3]).

useworker(Function) ->
	StatechumRef=make_ref(),synapselauncher:find_statechum()!{self(),StatechumRef,getStatechumWorker},receive {StatechumRef,Pid} -> Ref = make_ref(),Function(Pid,Ref), Pid!{Ref,terminate} end.

%% @edoc checks whether the second string is contained in the first one
containsStrict(List,[])->true;
containsStrict([],List)->false;
containsStrict([H|T],[A|B]) when H =:= A ->containsStrict(T,B);
containsStrict([H|T],[A|B]) when H =/= A ->false.

containsCast(A,B,C) -> % Accounts for different exception messages between OpenJDK and Oracle JDK
    contains(A,B ++ "cannot be cast to" ++ C) or contains(A,B ++ "cannot be cast to class" ++ C).

contains(List,[]) -> true;
contains([],List) -> false;
contains([H|T],[A|B]) when H=:=A -> containsStrict(T,B) or contains(T,[A|B]);
contains([H|T],[A|B]) when H=/=A -> contains(T,[A|B]).

%% This one is only used for testing of notifications
handleNotifications(Ref,Counter) ->
	receive 
		{Ref,status,step,_} -> handleNotifications(Ref,Counter+1);
		{Ref,Pid,check} -> Pid!{Ref,Counter}
	end.
	
handleNotificationsAndRecordThem(Ref,Progress) ->
	receive 
	%% Based on http://stackoverflow.com/questions/588003/convert-an-integer-to-a-string-in-erlang
		{Ref,status,step,{S,Fsm,Reds}} -> handleNotificationsAndRecordThem(Ref,Progress ++ lists:flatten(io_lib:format(" ~w<~w><reds:~w>",[S,Fsm,Reds])) );
		{Ref,status,step,{S}} -> handleNotificationsAndRecordThem(Ref,Progress ++ lists:flatten(io_lib:format(" ~w",[S])));
		{Ref,status,step,A} -> throw("unexpected message " ++ lists:flatten(io_lib:format("~w",[A])) );
		{Ref,Pid,check} -> Pid!{Ref,Progress}
	end.
	
	
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
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,updateConfiguration,[{'learnerScoreMode','JUNK'}]},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"No enum const")) end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,updateConfiguration,[{'learnerScoreMode',"nonsense"}]},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"OtpErlangString cannot be cast")) end end) end,
		fun() -> useworker(fun(Pid,Ref) -> Pid!{Ref,updateConfiguration,[{'a','b'}]},receive {Ref,failure,Text} -> ?assertEqual(true,contains(Text,"unknown field a")) end end) end		
		
		%%?USEWORKER((Pid!{Ref,updateConfiguration,[]},receive {Ref,ok} ->ok end)) end
	]}}.








