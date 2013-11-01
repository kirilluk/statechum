%%% -------------------------------------------------------------------
%%% Author  : kirr
%%% Description : Runs traces on Erlang modules and reports results.
%%% Copyright (c) 2011 The University of Sheffield
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
%%% To run, use
%%% /cygdrive/c/Program\ Files/erl5.10.3/bin/erlc synapselauncher.erl testsynapselauncher.erl && /cygdrive/c/Program\ Files/erl5.10.3/bin/erl -eval 'testsynapselauncher:test(),halt().' -noshell
%%% or 
%%% erlc synapselauncher.erl testsynapselauncher.erl && erl -eval 'testsynapselauncher:test(),halt().' -noshell
-module(testsynapselauncher).

-include_lib("eunit/include/eunit.hrl").

-define(WIN32,{win32,version}).

convertPath_test_() ->
	[
		?_assertEqual([],synapselauncher:convertPath([],aa)) ,
		?_assertEqual([],synapselauncher:convertPath([],?WIN32)) ,
		?_assertEqual("pathA/pathB",synapselauncher:convertPath(["pathA/pathB"],aa)) ,
		?_assertEqual("pathA/pathB",synapselauncher:convertPath(["pathA/pathB"],?WIN32)) ,
		?_assertEqual("pathA/pathB:pC:pD",synapselauncher:convertPath(["pathA/pathB","pC","pD"],aa)) ,
		?_assertEqual("pathA/pathB;pC;pD",synapselauncher:convertPath(["pathA/pathB","pC","pD"],?WIN32)) ,
		?_assertEqual("pathA/pathB:pC",synapselauncher:convertPath(["pathA/pathB","pC"],aa)) ,
		?_assertEqual("pathA/pathB;pC",synapselauncher:convertPath(["pathA/pathB","pC"],?WIN32))
	].

-define(KeysWithPaths,['-cp','-Djava.library.path']).
	
validateOptions_test_() ->
	[
		?_assertEqual(ok,synapselauncher:validateOptions([{a,b}],[])),
		?_assertEqual(ok,synapselauncher:validateOptions([{a,b}],?KeysWithPaths)),
		?_assertEqual(ok,synapselauncher:validateOptions([],[])),
		?_assertEqual(ok,synapselauncher:validateOptions([],?KeysWithPaths)),
		?_assertError({invalidOption,_},synapselauncher:validateOptions(["a"],[])),
		?_assertError({invalidOption,_},synapselauncher:validateOptions([44],[])),
		?_assertError({invalidOption,_},synapselauncher:validateOptions([{3,4}],[])),
		?_assertError({invalidOption,_},synapselauncher:validateOptions([{"a",4}],[])),
		?_assertEqual(ok,synapselauncher:validateOptions([{a,'4'}],[])),
		?_assertError({invalidOption,_},synapselauncher:validateOptions([{a,[]}],?KeysWithPaths)),
		?_assertError({invalidOption,_},synapselauncher:validateOptions([{a,23}],?KeysWithPaths)),
		?_assertEqual(ok,synapselauncher:validateOptions([{'-cp',["e"]}],?KeysWithPaths)),
		?_assertEqual(ok,synapselauncher:validateOptions([{'-cp',["e","p"]}],?KeysWithPaths)),
		?_assertEqual(ok,synapselauncher:validateOptions([{'-cp',[]}],?KeysWithPaths)),
		?_assertError({invalidOption,_},synapselauncher:validateOptions([{'-cp',23}],?KeysWithPaths)),
		?_assertError({invalidOption,_},synapselauncher:validateOptions([{aa,[]}],?KeysWithPaths)),
		?_assertError({invalidOption,_},synapselauncher:validateOptions([{'-cp',23},{a,b}],?KeysWithPaths))
	].
	
-define(JavaDefaultsList, [
		{'-cp',["bin","lib/colt.jar"]},
		{'-Djava.library.path',["linear/.libs","smt/.libs"]},
		{'-DVIZ_CONFIG','erlang'},
		{'-Xmx','1500m'}]).
		
buildOptions_test_() ->
	[
		?_assertEqual(["-Djava.library.path=linear/.libs:smt/.libs","-Xmx1500m","-DVIZ_CONFIG=erlang","-cp","bin:lib/colt.jar"],synapselauncher:buildOptions([],?JavaDefaultsList,nothing)),
		?_assertEqual(["-Djava.library.path=linear/.libs:smt/.libs","-Xmx44","-DVIZ_CONFIG=erlang","-cp","bin:lib/colt.jar"],synapselauncher:buildOptions([{'-Xmx','44'}],?JavaDefaultsList,nothing)),
		?_assertEqual(["-Djava.library.path=linear/.libs:smt/.libs","-Xmx55","-DVIZ_CONFIG=erlang","-cp","bin:lib/colt.jar:whateverA:whateverB"],synapselauncher:buildOptions([{'-Xmx','44'},{'-Xmx','55'},{'-cp',["whateverA","whateverB"]}],?JavaDefaultsList,nothing)),
		?_assertEqual(["-Djava.library.path=linear/.libs:smt/.libs","-Xmx1500m","-DVIZ_CONFIG=erlang","-cp","bin:lib/colt.jar"],synapselauncher:buildOptions([{'-cp',[]}],?JavaDefaultsList,nothing))
	].