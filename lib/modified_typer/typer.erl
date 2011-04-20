%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%

%%--------------------------------------------------------------------
%% File        : typer.erl
%% Author      : Bingwen He <Bingwen.He@gmail.com>
%% Description : The main driver of the TypEr application
%%--------------------------------------------------------------------

%%
%%	MODIFIED BY KIRR for integration into the Statechum project.
%%

-module(typer). %% have to replace the origina typer module, otherwise all other modules call the original's error and halt my erl machine.

-export([start/3]).
-export([error/1, compile_error/1]).	% for error reporting

%% Takes an output of of code:lib_dir(typer) and appends the rest of the path to it.
-include_lib("typer/src/typer.hrl").
%% In order to find out where the .beam has come from, I can use code:get_object_code(typer_annotator).

%%--------------------------------------------------------------------

%% -D Analysis#typer_analysis.macros %% list of macros to be defined 
%% -I Analysis#typer_analysis.includes %% list of directories to be included
%% -T Args#args{trust = []} % list of files from -T
%% -r Args#args{analyzed_dir_r = []} % list of directories to process recursively
%% Args#args{analyze= []} % list of files to analyse

start(FilesToAnalyse,Plt,Outputmode) ->

  Files = lists:map(fun(F) -> 
	if 
		is_atom(F) -> F;
		true	-> list_to_atom(F)
	end end,FilesToAnalyse),
  Args = #args{analyze = FilesToAnalyse},
  Analysis = #typer_analysis{contracts = false,mode = ?SHOW,plt = Plt},

  TrustedFiles = typer_preprocess:get_all_files(Args, trust),
  Analysis1 = Analysis#typer_analysis{t_files = TrustedFiles},
  Analysis2 = extract(Analysis1),
%%  dbg:start(),dbg:tracer(),dbg:tpl(typer_preprocess, '_', []),dbg:p(all, c),
  All_Files = typer_preprocess:get_all_files(Args, analysis),
  Analysis3 = Analysis2#typer_analysis{ana_files = All_Files},
  Analysis4 = typer_info:collect(Analysis3),
  TypeInfo = get_type_info(Analysis4),
%%%dbg:start(),dbg:tracer(),dbg:tpl(typer_annotator_s, '_', []),dbg:p(all, c),
 typer_annotator_s:annotate(TypeInfo,Outputmode)
  %% io:format("\nTyper analysis finished\n"),
  .

%%--------------------------------------------------------------------

-spec extract(#typer_analysis{}) -> #typer_analysis{}.

extract(#typer_analysis{macros = Macros, includes = Includes,
			t_files = TFiles, trust_plt = TrustPLT} = Analysis) ->
  %% io:format("--- Extracting trusted typer_info... "),
  Ds = [{d, Name, Value} || {Name, Value} <- Macros],
  CodeServer = dialyzer_codeserver:new(),
  Fun =
    fun(File, CS) ->
	%% We include one more dir; the one above the one we are trusting
	%% E.g, for /home/tests/typer_ann/test.ann.erl, we should include
	%% /home/tests/ rather than /home/tests/typer_ann/
	AllIncludes = [filename:dirname(filename:dirname(File)) | Includes],
	Is = [{i, Dir} || Dir <- AllIncludes],
	CompOpts = dialyzer_utils:src_compiler_opts() ++ Is ++ Ds,
	case dialyzer_utils:get_abstract_code_from_src(File, CompOpts) of
	  {ok, AbstractCode} -> 
	    case dialyzer_utils:get_record_and_type_info(AbstractCode) of
	      {ok, RecDict} ->
		Mod = list_to_atom(filename:basename(File, ".erl")),
		case dialyzer_utils:get_spec_info(Mod, AbstractCode, RecDict) of
		  {ok, SpecDict} ->
		    CS1 = dialyzer_codeserver:store_temp_records(Mod, RecDict, CS),
		    dialyzer_codeserver:store_temp_contracts(Mod, SpecDict, CS1);
		  {error, Reason} -> compile_error([Reason])
		end;
	      {error, Reason} -> compile_error([Reason])
	    end;
	  {error, Reason} -> compile_error(Reason)
	end
    end,
  CodeServer1 = lists:foldl(Fun, CodeServer, TFiles),
  %% Process remote types
  NewCodeServer =
    try
      NewRecords = dialyzer_codeserver:get_temp_records(CodeServer1),
      OldRecords = dialyzer_plt:get_types(TrustPLT), % XXX change to the PLT?
      MergedRecords = dialyzer_utils:merge_records(NewRecords, OldRecords),
      CodeServer2 = dialyzer_codeserver:set_temp_records(MergedRecords, CodeServer1),
      CodeServer3 = dialyzer_utils:process_record_remote_types(CodeServer2),
      dialyzer_contracts:process_contract_remote_types(CodeServer3)
    catch
      throw:{error, ErrorMsg} ->
	compile_error(ErrorMsg)
    end,
 %% Create TrustPLT
  Contracts = dialyzer_codeserver:get_contracts(NewCodeServer),
  Modules = dict:fetch_keys(Contracts),
  FoldFun =
    fun(Module, TmpPlt) ->
	{ok, ModuleContracts} = dict:find(Module, Contracts),
	SpecList = [{MFA, Contract} 
		    || {MFA, {_FileLine, Contract}} <- dict:to_list(ModuleContracts)],
	dialyzer_plt:insert_contract_list(TmpPlt, SpecList)
    end,
  NewTrustPLT = lists:foldl(FoldFun, TrustPLT, Modules),
  Analysis#typer_analysis{trust_plt = NewTrustPLT}.

%%--------------------------------------------------------------------

-spec get_type_info(#typer_analysis{}) -> #typer_analysis{}.

get_type_info(#typer_analysis{callgraph = CallGraph,
			      trust_plt = TrustPLT,
			      code_server = CodeServer} = Analysis) ->
  StrippedCallGraph = remove_external(CallGraph, TrustPLT),
  %% io:format("--- Analyzing callgraph... "),
  try 
    NewPlt = dialyzer_succ_typings:analyze_callgraph(StrippedCallGraph, 
						     TrustPLT, CodeServer),
    Analysis#typer_analysis{callgraph = StrippedCallGraph, trust_plt = NewPlt}
  catch
    error:What ->
      error(io_lib:format("Analysis failed with message: ~p", 
			  [{What, erlang:get_stacktrace()}]));
    throw:{dialyzer_succ_typing_error, Msg} ->
      error(io_lib:format("Analysis failed with message: ~s", [Msg]))
  end.

-spec remove_external(dialyzer_callgraph:callgraph(), dialyzer_plt:plt()) -> dialyzer_callgraph:callgraph().

remove_external(CallGraph, PLT) ->
  {StrippedCG0, Ext} = dialyzer_callgraph:remove_external(CallGraph),
  StrippedCG = dialyzer_callgraph:finalize(StrippedCG0),
  case get_external(Ext, PLT) of
    [] -> ok;
    Externals ->
      msg(io_lib:format(" Unknown functions: ~p\n", [lists:usort(Externals)]))
  end,
  StrippedCG.

-spec get_external([{mfa(), mfa()}], dialyzer_plt:plt()) -> [mfa()].

get_external(Exts, Plt) ->
  Fun = fun ({_From, To = {M, F, A}}, Acc) ->
	    case dialyzer_plt:contains_mfa(Plt, To) of
	      false ->
		case erl_bif_types:is_known(M, F, A) of
		  true -> Acc;
		  false -> [To|Acc]
		end;
	      true -> Acc
	    end
	end,
  lists:foldl(Fun, [], Exts).

%%--------------------------------------------------------------------

-spec error(string()) -> no_return().

error(Slogan) ->
  msg(io_lib:format("typer failure: ~s~n~n", [Slogan])),
  erlang:error(Slogan).

%%--------------------------------------------------------------------

-spec compile_error([string()]) -> no_return().

compile_error(Reason) ->
  JoinedString = lists:flatten([X ++ "\n" || X <- Reason]),
  Msg = "Analysis failed with error report:\n" ++ JoinedString,
  error(Msg).

%%--------------------------------------------------------------------
%% Outputs a message on 'stderr', if possible.
%%--------------------------------------------------------------------

-spec msg(string()) -> 'ok'.

msg(Msg) ->
  case os:type() of
    {unix, _} ->
      P = open_port({fd, 0, 2}, [out]),
      port_command(P, Msg),
      true = port_close(P),
      ok;
    _ ->  % win32, vxworks
      io:format("~s", [Msg])
  end.

%%--------------------------------------------------------------------
