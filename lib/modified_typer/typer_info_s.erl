%% -*- erlang-indent-level: 2 -*-
%%
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
%%
%%	MODIFIED for integration into the Statechum project.
%%  The two files which were modified for Statechum are typer.erl, typer_annotator.erl, typer_info_s.erl
%%  the rest were renamed to ensure no clashes with the installed typer.
%%

-module(typer_info_s).

-export([collect/1]).

-type func_info() :: {non_neg_integer(), atom(), arity()}.
-type inc_file_info() :: {string(), func_info()}.

-record(tmpAcc, {file		:: string(),
		 module		:: atom(),
		 funcAcc=[]	:: [func_info()],
		 incFuncAcc=[]	:: [inc_file_info()],
		 dialyzerObj=[] :: [{mfa(), {_, _}}]}).

-include("typer_s.hrl").

-spec collect(#typer_analysis{}) -> #typer_analysis{}.

collect(Analysis) ->
  NewPlt =
    try get_dialyzer_plt(Analysis) of
	DialyzerPlt ->
	dialyzer_plt:merge_plts([Analysis#typer_analysis.trust_plt, DialyzerPlt])
    catch
      throw:{dialyzer_error,Reason} ->
		typer_s:reportProblem('cannotLoadPlt')
    end,
  NewAnalysis = lists:foldl(fun collect_one_file_info/2, 
			    Analysis#typer_analysis{trust_plt = NewPlt}, 
			    Analysis#typer_analysis.ana_files),
  %% Process Remote Types
  TmpCServer = NewAnalysis#typer_analysis.code_server,
  NewCServer =
    try
      NewRecords = dialyzer_codeserver:get_temp_records(TmpCServer),
      OldRecords = dialyzer_plt:get_types(NewPlt),
      MergedRecords = dialyzer_utils:merge_records(NewRecords, OldRecords),
      %% io:format("Merged Records ~p",[MergedRecords]),
      TmpCServer1 = dialyzer_codeserver:set_temp_records(MergedRecords, TmpCServer),
      TmpCServer2 = dialyzer_utils:process_record_remote_types(TmpCServer1),
      dialyzer_contracts:process_contract_remote_types(TmpCServer2)
    catch
      throw:{error, ErrorMsg} ->
	typer_s:reportError(ErrorMsg)
    end,
  NewAnalysis#typer_analysis{code_server = NewCServer}.

collect_one_file_info(File, Analysis) ->
  Ds = [{d,Name,Val} || {Name,Val} <- Analysis#typer_analysis.macros],
  %% Current directory should also be included in "Includes".
  Includes = [filename:dirname(File)|Analysis#typer_analysis.includes],
  Is = [{i,Dir} || Dir <- Includes],
  Options = dialyzer_utils:src_compiler_opts() ++ Is ++ Ds,
  case dialyzer_utils:get_abstract_code_from_src(File, Options) of
    {error, Reason} ->
      %% io:format("File=~p\n,Options=~p\n,Error=~p\n", [File,Options,Reason]),
      typer_s:compile_error(Reason);
    {ok, AbstractCode} ->
      case dialyzer_utils:get_core_from_abstract_code(AbstractCode, Options) of
	error -> typer_s:compile_error(["Could not get core erlang for "++File]);
	{ok, Core} ->
	  case dialyzer_utils:get_record_and_type_info(AbstractCode) of
	    {error, Reason} -> typer_s:compile_error([Reason]);
	    {ok, Records} -> 
	      Mod = list_to_atom(filename:basename(File, ".erl")),
	      case dialyzer_utils:get_spec_info(Mod, AbstractCode, Records) of
		{error, Reason} -> typer_s:compile_error([Reason]);
		{ok, SpecInfo} -> 
		  analyze_core_tree(Core, Records, SpecInfo, Analysis, File);
		%% new argument is part of Erlang 5.9.1
		{ok, SpecInfo, CbInfo} ->
                  ExpTypes = get_exported_types_from_core(Core),
		  analyze_core_tree(Core, Records, SpecInfo, CbInfo,
				    ExpTypes, Analysis, File)
	      end
	  end
      end
  end.

analyze_core_tree(Core, Records, SpecInfo, Analysis, File) ->
  Module = list_to_atom(filename:basename(File, ".erl")),
  TmpTree = cerl:from_records(Core),
  CS1 = Analysis#typer_analysis.code_server,
  NextLabel = dialyzer_codeserver:get_next_core_label(CS1),
  {Tree, NewLabel} = cerl_trees:label(TmpTree, NextLabel),
  CS2 = dialyzer_codeserver:insert(Module, Tree, CS1),
  CS3 = dialyzer_codeserver:set_next_core_label(NewLabel, CS2),
  CS4 = dialyzer_codeserver:store_temp_records(Module, Records, CS3),
  CS5 = dialyzer_codeserver:store_temp_contracts(Module, SpecInfo, CS4),
  Ex_Funcs = [{0,F,A} || {_,_,{F,A}} <- cerl:module_exports(Tree)],
  TmpCG = Analysis#typer_analysis.callgraph,
  CG = dialyzer_callgraph:scan_core_tree(Tree, TmpCG),
  Fun = fun analyze_one_function/2,
  All_Defs = cerl:module_defs(Tree),
  Acc = lists:foldl(Fun, #tmpAcc{file=File, module=Module}, All_Defs),
  Exported_FuncMap = typer_map_s:insert({File, Ex_Funcs},
				      Analysis#typer_analysis.ex_func),
  %% NOTE: we must sort all functions in the file which
  %% originate from this file by *numerical order* of lineNo
  Sorted_Functions = lists:keysort(1, Acc#tmpAcc.funcAcc),
  FuncMap = typer_map_s:insert({File, Sorted_Functions},
			     Analysis#typer_analysis.func),
  %% NOTE: However we do not need to sort functions
  %% which are imported from included files.
  IncFuncMap = typer_map_s:insert({File, Acc#tmpAcc.incFuncAcc}, 
				Analysis#typer_analysis.inc_func),
  Final_Files = Analysis#typer_analysis.final_files ++ [{File, Module}],
  RecordMap = typer_map_s:insert({File, Records}, Analysis#typer_analysis.record),
  Analysis#typer_analysis{final_files=Final_Files,
			  callgraph=CG,
			  code_server=CS5,
			  ex_func=Exported_FuncMap,
			  inc_func=IncFuncMap,
			  record=RecordMap,
			  func=FuncMap}.

analyze_one_function({Var, FunBody} = Function, Acc) ->
  F = cerl:fname_id(Var),
  A = cerl:fname_arity(Var),
  TmpDialyzerObj = {{Acc#tmpAcc.module, F, A}, Function},
  NewDialyzerObj = Acc#tmpAcc.dialyzerObj ++ [TmpDialyzerObj],  
  [_, LineNo, {file, FileName}] = cerl:get_ann(FunBody),
  BaseName = filename:basename(FileName),
  FuncInfo = {LineNo, F, A},
  OriginalName = Acc#tmpAcc.file,
  {FuncAcc, IncFuncAcc} =
    case (FileName =:= OriginalName) orelse (BaseName =:= OriginalName) of
      true -> %% Coming from original file
	%% io:format("Added function ~p\n", [{LineNo, F, A}]),
	{Acc#tmpAcc.funcAcc ++ [FuncInfo], Acc#tmpAcc.incFuncAcc};
      false ->
	%% Coming from other sourses, including:
	%%     -- .yrl (yecc-generated file)
	%%     -- yeccpre.hrl (yecc-generated file)
	%%     -- other cases
	{Acc#tmpAcc.funcAcc, Acc#tmpAcc.incFuncAcc ++ [{FileName, FuncInfo}]}
    end,
  Acc#tmpAcc{funcAcc = FuncAcc,
	     incFuncAcc = IncFuncAcc,
	     dialyzerObj = NewDialyzerObj}.

get_dialyzer_plt(#typer_analysis{plt = PltFile0}) ->
  PltFile =
    case PltFile0 =:= none of
      true -> dialyzer_plt:get_default_plt();
      false -> PltFile0
    end,
  dialyzer_plt:from_file(PltFile).
  
%% These are components of typer.erl from Erlang 5.9.1
get_exported_types_from_core(Core) ->
  Attrs = cerl:module_attrs(Core),
  ExpTypes1 = [cerl:concrete(L2) || {L1, L2} <- Attrs,
                                    cerl:is_literal(L1),
                                    cerl:is_literal(L2),
                                    cerl:concrete(L1) =:= 'export_type'],
  ExpTypes2 = lists:flatten(ExpTypes1),
  M = cerl:atom_val(cerl:module_name(Core)),
  sets:from_list([{M, F, A} || {F, A} <- ExpTypes2]).

analyze_core_tree(Core, Records, SpecInfo, CbInfo, ExpTypes, Analysis, File) ->
  Module = cerl:concrete(cerl:module_name(Core)),
  TmpTree = cerl:from_records(Core),
  CS1 = Analysis#typer_analysis.code_server,
  NextLabel = dialyzer_codeserver:get_next_core_label(CS1),
  {Tree, NewLabel} = cerl_trees:label(TmpTree, NextLabel),
  CS2 = dialyzer_codeserver:insert(Module, Tree, CS1),
  CS3 = dialyzer_codeserver:set_next_core_label(NewLabel, CS2),
  CS4 = dialyzer_codeserver:store_temp_records(Module, Records, CS3),
  CS5 =
    case Analysis#typer_analysis.no_spec of
      true -> CS4;
      false ->
	dialyzer_codeserver:store_temp_contracts(Module, SpecInfo, CbInfo, CS4)
    end,
  OldExpTypes = dialyzer_codeserver:get_temp_exported_types(CS5),
  MergedExpTypes = sets:union(ExpTypes, OldExpTypes),
  CS6 = dialyzer_codeserver:insert_temp_exported_types(MergedExpTypes, CS5),
  Ex_Funcs = [{0,F,A} || {_,_,{F,A}} <- cerl:module_exports(Tree)],
  TmpCG = Analysis#typer_analysis.callgraph,
  CG = dialyzer_callgraph:scan_core_tree(Tree, TmpCG),
  Fun = fun analyze_one_function/2,
  All_Defs = cerl:module_defs(Tree),
  Acc = lists:foldl(Fun, #tmpAcc{file = File, module = Module}, All_Defs),
  Exported_FuncMap = typer_map_s:insert({File, Ex_Funcs},
				      Analysis#typer_analysis.ex_func),
  %% we must sort all functions in the file which
  %% originate from this file by *numerical order* of lineNo
  Sorted_Functions = lists:keysort(1, Acc#tmpAcc.funcAcc),
  FuncMap = typer_map_s:insert({File, Sorted_Functions},
			     Analysis#typer_analysis.func),
  %% we do not need to sort functions which are imported from included files
  IncFuncMap = typer_map_s:insert({File, Acc#tmpAcc.incFuncAcc}, 
				Analysis#typer_analysis.inc_func),
  RecordMap = typer_map_s:insert({File, Records}, Analysis#typer_analysis.record),
  Final_Files = Analysis#typer_analysis.final_files ++ [{File, Module}],
  Analysis#typer_analysis{final_files = Final_Files,
		    callgraph = CG,
		    code_server = CS6,
		    ex_func = Exported_FuncMap,
		    inc_func = IncFuncMap,
		    record = RecordMap,
		    func = FuncMap}.
