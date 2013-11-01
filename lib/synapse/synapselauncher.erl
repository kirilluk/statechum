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

-module(synapselauncher).

-export([launch/4,convertPath/2,validateOptions/2,constructNodeDetails/0,buildOptions/3]).
-include("synapse.hrl").

%%% @doc Starts the Java runtime with Statechum
%%% Java is a full path to the Java runtime, 
%%% StatechumDir is the directory with Statechum, 
%%% Args are the arguments, both to the interpreter and to the process.
%%% AccumulateOutput is whether all output from the Java runtime is to be collected and returned from launch.
%%%
%%% Process arguments are the following: ourNode, cookie, mailBox. These are used for communication with the node set up by the Java runtime.

validateOptions(Options,ListOptions) ->
	lists:foreach(fun(K) -> 
		case K of
			{Key,Value} ->
				case is_atom(Key) of
					true -> 
						case lists:member(Key,ListOptions) of
						true -> 
							case is_list(Value) of
									true-> ok;
									false -> error({invalidOption,"Key "++atom_to_list(Key)++" should have a list value"})
								end;
						false ->
								case is_atom(Value) of
									false -> error({invalidOption,"Key "++atom_to_list(Key)++" should have an atom value"});
									true -> ok
								end
						end;
					false ->
						error({invalidOption,"Key is not an atom"})
				end;
			_ -> error({invalidOption,"Tuple is not key-value pair"})
		end
	end,Options).

convertPath([],_) -> "";
convertPath([Path],_) -> Path;
convertPath([H|OtherComponents],OsType) ->
	case OsType of
		{win32,_} -> H ++ ";" ++ convertPath(OtherComponents,OsType);
		_ -> H	++ ":" ++ convertPath(OtherComponents,OsType)
	end.


buildOptions(JavaOptionsList,JavaDefaultsList,OsType) ->
	KeysWithPaths=['-cp','-Djava.library.path'],
	validateOptions(JavaDefaultsList,KeysWithPaths),validateOptions(JavaOptionsList,KeysWithPaths),
	JavaDefaultsDict=dict:from_list(JavaDefaultsList),JavaOptionsDict=dict:from_list(JavaOptionsList),
	MergedOptions = dict:merge(fun(K,V1,V2) -> 
		case lists:member(K,KeysWithPaths) of
			true -> V1 ++ V2;
			false ->V2
		end
	end,JavaDefaultsDict,JavaOptionsDict),
	%%% Java command line options are inconsistent, hence to build them here we have to include special cases for every option not in the form of name=value.
	dict:fold(fun(K,V,Acc) -> Acc ++ 
		case K of
			'-Xmx' -> ["-Xmx" ++ atom_to_list(V)];
			'-Xms' -> ["-Xms" ++ atom_to_list(V)];
			'-cp' -> ["-cp",convertPath(V,OsType) ];
			_ ->
				[atom_to_list(K) ++ "=" ++
				case lists:member(K,KeysWithPaths) of
					true->convertPath(V,OsType);
					false->atom_to_list(V)
				end]
		end
	end,[],MergedOptions).

waitForJavaNode(Node,0) ->
	throw("Timeout waiting for node " ++ atom_to_list(Node) ++ " to start");
	
waitForJavaNode(Node,HowLong) ->
	timer:sleep(500),
	case (net_adm:ping(Node)) of
		pong -> true;
		pang -> waitForJavaNode(Node,HowLong-1)
	end.


	
constructNodeDetails() ->
	case inet:gethostname() of
		{ok,HostName} ->
			{list_to_atom("statechum" ++ os:getpid() ++ "@" ++ HostName), 'statechum'}
	end.

%%% this one needs R_HOME to be set
%%% need to test with the wrong value of ref returned
launch(Java,StatechumDir,JavaOptionsList, AccumulateOutput) ->
	JavaDefaultsList = [
		{'-cp',["bin","lib/colt.jar","lib/commons-collections-3.1.jar","lib/jung-1.7.6.jar","lib/sootclasses.jar","lib/jltl2ba.jar","lib/OtpErlang.jar","lib/junit-4.8.1.jar","lib/javaGD.jar","lib/JRI.jar","lib/weka.jar"]},
		{'-Djava.library.path',["linear/.libs","smt/.libs"]},
		{'-DVIZ_CONFIG','erlang'},
		{'-DVIZ_DIR','resources/graphLayout'},
		{'-Dthreadnum',list_to_atom(lists:flatten(io_lib:format("~p", [erlang:system_info(logical_processors_available)])))},
		{'-Xmx','1500m'}],
	case constructNodeDetails() of
		{NodeName,PidName} ->
			Port = open_port({spawn_executable, Java}, [hide,in,stderr_to_stdout,use_stdio,{cd, StatechumDir},{args,["-ea"] ++ buildOptions(JavaOptionsList,JavaDefaultsList,os:type()) ++ 
				["statechum.analysis.Erlang.Synapse", atom_to_list(NodeName), atom_to_list(erlang:get_cookie()), atom_to_list(PidName)]}]),
			process_flag(trap_exit, true),
			waitForJavaNode(NodeName,10),
			Ref = make_ref(),
			{PidName,NodeName}!{self(),Ref,echo,someData},
			receive
				{Ref, ok, Answer} ->ok
				after 3000 ->
					throw("Timeout waiting for response")
			end,
%%			{PidName,NodeName}!{self(),Ref,terminate,someData},
%%			spawn(fun() -> timer:sleep(1000),{PidName,NodeName}!{self(),Ref,terminate,someData} end),
			spawn(fun() -> timer:sleep(1000),{PidName,NodeName}!{self(),Ref,terminate,someData} end),
			loop(Port,"completed",false)
	end.

%%% Given text output from the process, either accumulates it and returns the result or simply dumps it into the standard output.
loop(Port,ResponseAsText,AccumulateOutput) ->
	receive
		{Port, {data, Data}} ->
			case AccumulateOutput of
				true -> loop(Port,ResponseAsText ++ Data,AccumulateOutput);
				false-> io:format("~s", [Data]),loop(Port,ResponseAsText,AccumulateOutput)
			end;
		{'EXIT', Port, _} ->
			ResponseAsText
	end.
