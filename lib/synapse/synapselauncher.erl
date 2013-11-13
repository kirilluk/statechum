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

-module(synapselauncher).

-export([startStatechum/1,convertPath/2,validateOptions/2,constructNodeDetails/0,buildOptions/3,find_statechum/0]).
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


mergeOptions(OptionsList,DefaultsList,KeysWithPaths) ->
	validateOptions(DefaultsList,KeysWithPaths),validateOptions(OptionsList,KeysWithPaths),
	DefaultsDict=dict:from_list(DefaultsList),OptionsDict=dict:from_list(OptionsList),
	dict:merge(fun(K,V1,V2) -> 
		case lists:member(K,KeysWithPaths) of
			true -> V1 ++ V2;
			false ->V2
		end
	end,DefaultsDict,OptionsDict).
	
buildOptions(JavaOptionsList,JavaDefaultsList,OsType) ->
	KeysWithPaths = ['-cp','-Djava.library.path'],
	MergedOptions = mergeOptions(JavaOptionsList,JavaDefaultsList,KeysWithPaths),
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

-define(StatechumName,'javaStatechumProcess').

obtainCookie(MergedOptions) ->
	case dict:find('Cookie',MergedOptions) of
		{ok,Value} -> Value; %% Value is an atom, otherwise options validation will fail.
		_ ->erlang:get_cookie()
	end.

%%% this one needs R_HOME to be set /library/rJava/jri/x64
%%% need to test with the wrong value of ref returned
launch(OptionsList,PidToNotify) ->
	DefaultsList = [
		{'Java','java'}, %% Path to Java executable
		{'StatechumDir','.'}, %% Path to Statechum
		{'AccumulateOutput','false'}, %% only used for testing, this one will cause this process to report all output Java dumps on the screen
		{'JavaOptionsList',[]}], %% Java options, defaults below.
	MergedOptions = mergeOptions(OptionsList,DefaultsList,['JavaOptionsList']),
	{Java,StatechumDir,JavaOptionsList}={dict:fetch('Java',MergedOptions),dict:fetch('StatechumDir',MergedOptions),dict:fetch('JavaOptionsList',MergedOptions)},
	
	%% if R_HOME is defined in the options, move it to the list of environmental arguments. One would additionally have to set an option to java.library.path
	OtherOptions=case dict:find('R_HOME',MergedOptions) of
		{ok,Value} -> [{env,[{'R_HOME',Value}]}]; %% Value is an atom, otherwise options validation will fail.
		error ->[]
	end,
	
	Cookie = obtainCookie(MergedOptions),
	CurrentCookie = erlang:get_cookie(),
	if 
		Cookie =/= CurrentCookie -> erlang:set_cookie(Cookie);
		true -> ok
	end,
	
	%% now merge Java options.
	JavaDefaultsList = [
		{'-cp',["bin","lib/colt.jar","lib/commons-collections-3.1.jar","lib/jung-1.7.6.jar","lib/sootclasses.jar","lib/jltl2ba.jar","lib/OtpErlang.jar","lib/junit-4.8.1.jar","lib/javaGD.jar","lib/JRI.jar","lib/weka.jar"]},
		{'-Djava.library.path',["linear/.libs","smt/.libs"]},
		{'-DESC_TERMINATE','false'},%% stop ESC-termination of Java runtime from within Visualser windows
		{'-DVIZ_CONFIG','erlang'}, %% the name of file where to store window layouts
		{'-DVIZ_DIR','.'}, %% this is usually resources/graphLayout but most likely this path will not be available hence layouts will be dumped locally
		{'-Dthreadnum',list_to_atom(lists:flatten(io_lib:format("~p", [erlang:system_info(logical_processors_available)])))},
		{'-Xmx','1500m'}],
	FullJavaOptions = buildOptions(JavaOptionsList,JavaDefaultsList,os:type()),
	case constructNodeDetails() of
		{NodeName,PidName} ->
			Port = open_port({spawn_executable, Java}, [hide,in,stderr_to_stdout,use_stdio,{cd, StatechumDir}] ++ OtherOptions ++[{args,["-ea"] ++ FullJavaOptions ++ 
				["statechum.analysis.Erlang.Synapse", atom_to_list(NodeName), atom_to_list(Cookie), atom_to_list(PidName), node()]}]),
			process_flag(trap_exit, true),
			waitForJavaNode(NodeName,10),
			Ref = make_ref(),
			{PidName,NodeName}!{self(),Ref,echo},
			%%loop(Port,[],aa,PidToNotify,false),
			receive
				{Ref, ok, Pid} ->
					%% At this point, we have started Statechum in a Java node and established communication with it.
					PidToNotify!ok,
					loop(Port,[],Pid,PidToNotify,dict:fetch('AccumulateOutput',MergedOptions))
				%{'EXIT', Port, _} ->throw("unexpected Java termination")
				%The above is commented out because all we care about is whether we get a response and forcing the unexpected termination to occur under test requires it to terminate when we sent it a specific message (but also happens on its own on slow PCs).
				after 1000 ->
					throw("Timeout waiting for echo response")
			end
			%% An alternative to sending Pid from Statechum is rpc:call(Node, erlang, whereis, [RegisteredName]) (thanks to http://stackoverflow.com/questions/16977972/how-to-exit-remote-pid-given-node-name-and-registered-name )
			
			
%%			loop(Port,"completed",NodeName,PidName,false)
%%			{PidName,NodeName}!{self(),Ref,terminate,someData},
%%			spawn(fun() -> timer:sleep(1000),{PidName,NodeName}!{self(),Ref,terminate} end),
%%			PID = spawn(fun() -> timer:sleep(1000),{PidName,NodeName}!{self(),Ref,terminate} end),
%%			link(PID),
	end.

startStatechum(OptionsList) ->
	case whereis(?StatechumName) of
		undefined ->
				ThisProcess = self(),
				Pid = spawn_link(fun() -> launch(OptionsList,ThisProcess) end),
				receive %% here we either wait to receive an ok message or to be killed if Statechum fails to start
				    ok -> register(?StatechumName,Pid),
					  Pid
				end;
		Pid -> Pid
	end.

find_statechum() ->
	case whereis(?StatechumName) of
		Pid when is_pid(Pid) -> Pid;
		_ -> not_started
	end.

%%% Given text output from the process, either accumulates it and returns the result or simply dumps it into the standard output.
loop(Port,ResponseAsText,Pid,ParentPid,AccumulateOutput) ->
	receive
		{Port, {data, Data}} ->
			case AccumulateOutput of
				true -> loop(Port,ResponseAsText ++ Data,Pid,ParentPid,AccumulateOutput);
				false-> io:format(user,"~s", [Data]),loop(Port,ResponseAsText,Pid,ParentPid,AccumulateOutput)
			end;
		%% if we are accumulating output (aka running under test), we expect to get some within a relatively short period of time, report a failure if there is none forthcoming.
		{ResponsePid,Ref,Command} when is_pid(ResponsePid),is_reference(Ref),is_atom(Command) -> Pid!{ResponsePid,Ref,Command},loop(Port,ResponseAsText,Pid,ParentPid,AccumulateOutput);
		{'EXIT', ParentPid, _ } when is_pid(ParentPid) -> Pid!{ParentPid,make_ref(),terminate},loop(Port,[],Pid,ParentPid,AccumulateOutput); %% if our parent terminated, ask Statechum to terminate and wait for it but not accumulating anything.
		{'EXIT', Port, _} when is_port(Port) ->%% port_close(Port) will fail here because port is already closed 
			if AccumulateOutput == true -> ParentPid!ResponseAsText,ok; true -> ok end;
		terminate -> Pid!{ParentPid,make_ref(),terminate},loop(Port,ResponseAsText,Pid,ParentPid,AccumulateOutput) %% here we expect Statechum to terminate and output to become available.
%%		after 3000 -> 
%%			if AccumulateOutput == true -> Pid!terminate,throw("Timeout waiting for any response");true -> loop(Port,ResponseAsText,Pid,ParentPid,AccumulateOutput)  end
	end.

	
