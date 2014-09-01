%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
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
%%============================================================================
%% File    : typer_annotator.erl
%% Author  : Bingwen He <hebingwen@hotmail.com>
%% Description : 
%%    If file 'FILENAME' has been analyzed, then the output of
%%    command "diff -B FILENAME.erl typer_ann/FILENAME.ann.erl"
%%    should be exactly what TypEr has added, namely type info.
%%============================================================================
%%
%%	MODIFIED for integration into the Statechum project.
%%  The two files which were modified for Statechum are typer.erl and typer_annotator.erl,
%%  the rest were renamed to ensure no clashes with the installed typer.
%%

-module(typer_annotator_s).

-export([annotate/2,t_to_Statechum/2]).

-include("erltypes.hrl").%% the value of this define is set at a command line.

%%----------------------------------------------------------------------------

%% Takes an output of of code:lib_dir(typer) and appends the rest of the path to it.
%-include_lib("typer/src/typer.hrl").
%% In order to find out where the .beam has come from, I can use code:get_object_code(typer_annotator).

-include("typer_s.hrl").

%%----------------------------------------------------------------------------

-define(TYPER_ANN_DIR, "typer_ann").

-type func_info() :: {non_neg_integer(), atom(), arity()}.

-record(info, {recMap = typer_map_s:new() :: dict(),
	       funcs = []               :: [func_info()],
	       typeMap                  :: dict(),
	       contracts                :: boolean()}).
-record(inc, {map    = typer_map_s:new() :: dict(),
	      filter = []              :: [string()]}).

%%----------------------------------------------------------------------------

-spec annotate(#typer_analysis{},atom()) -> 'ok'.

annotate(#typer_analysis{mode = ?SHOW} = Analysis,OutputMode) ->
    show(Analysis,OutputMode).


write_inc_files(Inc) ->
  Fun =
    fun (File) ->
	Val = typer_map_s:lookup(File,Inc#inc.map),
	%% Val is function with its type info
	%% in form [{{Line,F,A},Type}]
	Functions = [Key || {Key,_} <- Val],
	Val1 = [{{F,A},Type} || {{_Line,F,A},Type} <- Val],
	Info = #info{typeMap = typer_map_s:from_list(Val1),
		     recMap = typer_map_s:new(),
		     %% Note we need to sort functions here!
		     funcs = lists:keysort(1, Functions)}
	%% io:format("TypeMap ~p\n", [Info#info.typeMap]),
	%% io:format("Funcs ~p\n", [Info#info.funcs]),
	%% io:format("RecMap ~p\n", [Info#info.recMap]),
    end,
  lists:foreach(Fun, dict:fetch_keys(Inc#inc.map)).

show(Analysis,Outputmode) ->
  Fun = fun({File, Module},Acc) -> 
	    Info = get_final_info(File, Module, Analysis),
	    case(Outputmode) of
		text ->
	    		Acc ++ show_type_info_only(File, Info);
		types-> Acc ++ [{File, Module,Info#info.recMap, extract_type_info(File,Info) }];
		_ -> typer_s:reportError("unknown mode of output")
	    end
	end,
  lists:foldl(Fun, [], Analysis#typer_analysis.final_files).

get_final_info(File, Module, Analysis) ->
  RecMap = get_recMap(File, Analysis),
  TypeMap = get_typeMap(Module, Analysis,RecMap),
  Functions = get_functions(File, Analysis),
  Contracts = Analysis#typer_analysis.contracts,
  #info{recMap=RecMap, funcs=Functions, typeMap=TypeMap, contracts=Contracts}.

get_recMap(File, Analysis) ->
  typer_map_s:lookup(File, Analysis#typer_analysis.record).

get_typeMap(Module, Analysis, RecMap) ->
  TypeInfoPlt = Analysis#typer_analysis.trust_plt,
  TypeInfo = 
    case dialyzer_plt:lookup_module(TypeInfoPlt, Module) of
      none -> [];
      {value, List} -> List
    end,
  CodeServer = Analysis#typer_analysis.code_server,
  TypeInfoList = [get_type(I, CodeServer, RecMap) || I <- TypeInfo],
  typer_map_s:from_list(TypeInfoList).

get_type({{M, F, A} = MFA, Range, Arg}, CodeServer, RecMap) ->
  case dialyzer_codeserver:lookup_mfa_contract(MFA, CodeServer) of
    error ->
      {{F, A}, {Range, Arg}};
    {ok, {_FileLine, Contract}} ->
      Sig = erl_types:t_fun(Arg, Range),
      case dialyzer_contracts:check_contract(Contract, Sig) of
	ok -> {{F, A}, {contract, Contract}};
	{error, {extra_range, _, _}} ->
	  {{F, A}, {contract, Contract}};
	{error, invalid_contract} ->
	  CString = dialyzer_contracts:contract_to_string(Contract),
	  SigString = dialyzer_utils:format_sig(Sig, RecMap),
	  typer_s:reportError(
	    io_lib:format("Error in contract of function ~w:~w/~w\n" 
			  "\t The contract is: " ++ CString ++ "\n" ++
			  "\t but the inferred signature is: ~s",
			  [M, F, A, SigString]));
	{error, Msg} when is_list(Msg) -> % Msg is a string()
	  typer_s:reportError(
	    io_lib:format("Error in contract of function ~w:~w/~w: ~s",
			  [M, F, A, Msg]))
      end
  end.

get_functions(File, Analysis) ->
  case Analysis#typer_analysis.mode of
    ?SHOW ->
      Funcs = typer_map_s:lookup(File, Analysis#typer_analysis.func),
      Inc_Funcs = typer_map_s:lookup(File, Analysis#typer_analysis.inc_func),
      remove_module_info(Funcs) ++ normalize_incFuncs(Inc_Funcs);
    ?SHOW_EXPORTED ->
      Ex_Funcs = typer_map_s:lookup(File, Analysis#typer_analysis.ex_func),
      remove_module_info(Ex_Funcs);
    ?ANNOTATE ->
      Funcs = typer_map_s:lookup(File, Analysis#typer_analysis.func),
      remove_module_info(Funcs);
    ?ANNOTATE_INC_FILES ->
      typer_map_s:lookup(File, Analysis#typer_analysis.inc_func)
  end.

normalize_incFuncs(Funcs) ->
  [FuncInfo || {_FileName, FuncInfo} <- Funcs].

-spec remove_module_info([func_info()]) -> [func_info()].

remove_module_info(FuncInfoList) ->
  F = fun ({_,module_info,0}) -> false;
	  ({_,module_info,1}) -> false;
	  ({Line,F,A}) when is_integer(Line), is_atom(F), is_integer(A) -> true
      end,
  lists:filter(F, FuncInfoList).

get_type_string(F, A, Info, Mode) ->
  Type = get_type_info({F,A}, Info#info.typeMap),
  TypeStr =
    case Type of
      {contract, C} -> 
        dialyzer_contracts:contract_to_string(C);
      {RetType, ArgType} ->
        dialyzer_utils:format_sig(erl_types:t_fun(ArgType, RetType),
				  Info#info.recMap)
    end,

  case {Mode, Type} of
    {file, {contract, _}} -> 
		typer_s:reportError("type of " ++ atom_to_list(F) ++ "is a file contract");
    _ ->
	Prefix = lists:concat(["-spec ", F,TypeStr])
  end.
 
show_type_info_only(File, Info) ->
  Underline = lists:flatten(io_lib:format(lists:concat(["~", length(File)+8, "c~n"]),[$-])),
  Fun = fun ({_LineNo, F, A},Acc) ->
	    TypeInfo = get_type_string(F, A, Info, show),
	    Acc ++ TypeInfo++".\n"
	end,
  lists:foldl(Fun, "\n%% File: \"" ++ File  ++ "\"\n%% " ++ Underline, Info#info.funcs).

extract_type_info(File, Info) ->
  Fun = fun ({LineNo, F, A},Acc) ->
	Type = get_type_info({F,A}, Info#info.typeMap),
	Details = case Type of
	   {contract, C} -> 
			typer_s:reportError("type of " ++ atom_to_list(F) ++ "/" ++ integer_to_list(A) ++ "is a contract");
	   {RetType, ArgType} ->
			try
				FunctionType = fun_to_Statechum(erl_types:t_fun(ArgType, RetType),Info#info.recMap),
				{File, LineNo, F, A,FunctionType}
			catch
				throw:Str -> { F, A, Str }
			end
	end,
   	Acc ++ [Details]
  end,
  lists:foldl(Fun, [], Info#info.funcs).

get_type_info(Func, TypeMap) ->
  case typer_map_s:lookup(Func, TypeMap) of
    none ->
      %% Note: Typeinfo of any function should exist in
      %% the result offered by dialyzer, otherwise there 
      %% *must* be something wrong with the analysis
	{FuncName,ArgNum}=Func,
      	typer_s:reportError("No type info for function "++atom_to_list(FuncName) ++ "/" ++ integer_to_list(ArgNum) );
    {contract, _Fun} = C -> C;
    {_RetType, _ArgType} = RA -> RA 
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The main problem we are trying to solve here is turn the inferred
%% data type into something Statechum can comprehend, this is 
%% necessary in order to generate different permutations of values
%% for the purpose of inference.
%% Most of the code is nearly verbatim from erl_types:t_to_string
%% although most of types are mentioned at
%% http://www.erlang.org/eeps/eep-0008.html
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The format of the output is { JavaClass, ListOfAttributes, List1, List2 ... }
%% (it is not a record in order to permit variables number of elements and we'd best be 
%% explicit as to the values of every element).
%% Statechum instantiates the supplied class and passes it the args provided.
%% For a class name XX, statechum.analysis.Erlang.Signatures.XXSignature will be instantiated.

unsupportedType(Descr) -> throw("Unsupported type: "++Descr).

%% This one is used in two cases, to dump sets of atoms and sets of numbers
%% Returns a list of values
set_to_Statechum(Set) ->
  List = ordsets:to_list(Set),
  lists:foreach(fun(X) ->
	case is_atom(X) orelse is_number(X) of
		false -> typer_s:reportError(io_lib:format("Asked to dump element ~w of a set which is neither an atom nor a number",[X]));
		true ->true
	end end,List),
   List.

sequence_to_Statechum(Types, RecDict) ->
  [t_to_Statechum(T, RecDict) || T <- Types].

fun_to_Statechum(?function(?any, ?any), _RecDict) -> unsupportedType("cannot handle functions with unknown arity");
%%  "fun()";
fun_to_Statechum(?function(?any, _Range), _RecDict) -> unsupportedType("cannot handle functions with unknown arity");
%%  "fun((...) -> " ++ t_to_string(Range, RecDict) ++ ")";
fun_to_Statechum(?function(?product(ArgList), Range), RecDict) ->
 { 'Func',[],sequence_to_Statechum(ArgList, RecDict),t_to_Statechum(Range, RecDict) }.

t_to_Statechum(?any, _RecDict) ->
  {'Any',[]};
t_to_Statechum(?none, _RecDict) ->
  {'None',[]};
t_to_Statechum(?unit, _RecDict) -> unsupportedType("no_return");
%%  "no_return()";

t_to_Statechum(?atom(?any), _RecDict) -> 
  {'Atom',[]};
t_to_Statechum(?atom(Set), _RecDict) ->
  case ordsets:size(Set) of
    2 ->
      case ordsets:is_element(true, Set) andalso ordsets:is_element(false, Set) of
	true -> {'Boolean',[]};
	false -> { 'Atom',[],set_to_Statechum(Set) }
      end;
    _ ->
      { 'Atom',[],set_to_Statechum(Set) }
  end;

t_to_Statechum(?bitstr(U, B), _RecDict) -> {'BitString',[],[ U, B ]};

t_to_Statechum(?function(_, _), _RecDict) -> unsupportedType("functions as arguments are not yet supported");

t_to_Statechum(?identifier(Set), _RecDict) -> 
	SetPid = erl_types:t_is_pid(?identifier(Set)),SetPort = erl_types:t_is_port(?identifier(Set)),
	if
		SetPid  -> %%{'Pid',[]};
			unsupportedType("we cannot instantiate PIDs at the moment");
		SetPort -> %%{'Port',[]};
			unsupportedType("we cannot instantiate Ports (used to link to native code) at the moment");
		true -> unsupportedType("references are not supported")
	end;

%%  if Set =:= ?any -> "identifier()";
%%     true -> sequence([io_lib:format("~w()", [T]) 
%%		       || T <- ordsets:to_list(Set)], [], " | ")
%%  end;
%% t_to_string(?opaque(Set), _RecDict) ->
%%  sequence([case is_opaque_builtin(Mod, Name) of
%%	      true  -> io_lib:format("~w()", [Name]);
%%	      false -> io_lib:format("~w:~w()", [Mod, Name])
%%	    end
%%	    || #opaque{mod = Mod, name = Name} <- ordsets:to_list(Set)], [], " | ");

t_to_Statechum(?opaque(Set), _RecDict) -> unsupportedType("Opaque types cannot be created externally");
%%	{'Opaque', []};
	
t_to_Statechum(?matchstate(_Pres, _Slots), _RecDict) -> unsupportedType("matchstates are not supported");
%%  io_lib:format("ms(~s,~s)", [t_to_string(Pres, RecDict),
%%			      t_to_string(Slots,RecDict)]);
t_to_Statechum(?nil, _RecDict) -> {'String',[],[[]]};
%%  "[]";
t_to_Statechum(?nonempty_list(Contents, Termination), RecDict) ->
  StringContents = t_to_Statechum(Contents, RecDict),
  case Termination of
    ?nil ->
      case Contents of
	?char -> {'String',['nonempty']};%% "nonempty_string()";
	_ -> {'List',[],[StringContents]} %% "["++ContentString++",...]"
      end;
    ?any -> 
      %% Just a safety check.
      case Contents =:= ?any of
	true -> ok;
	false ->
	  typer_s:reportError({illegal_list, ?nonempty_list(Contents, Termination)})
      end,
      {'List',['nonempty','maybeimproper'],[]}; %% "nonempty_maybe_improper_list()";
    _ ->
      case erl_types:t_is_subtype(erl_types:t_nil(), Termination) of
	true ->
	  {'List',['nonempty','maybeimproper'],[StringContents,t_to_Statechum(Termination, RecDict)]};
	  %% "nonempty_maybe_improper_list("++ContentString++","++t_to_string(Termination, RecDict)++")";
	false ->
	  {'List',['nonempty','improper'],[StringContents,t_to_Statechum(Termination, RecDict)]}
	  %% "nonempty_improper_list("++ContentString++","++t_to_string(Termination, RecDict)++")"
      end
  end;
t_to_Statechum(?list(Contents, Termination, ?unknown_qual), RecDict) ->
  StringContents = t_to_Statechum(Contents, RecDict),
  case Termination of
    ?nil ->
      case Contents of
	?char -> {'String',[]};%% arbitrary string
	_ -> {'List',[],[StringContents]} %% "["++ContentString++"]"
      end;
    ?any ->
      %% Just a safety check.      
      case Contents =:= ?any of
	true -> ok;
	false ->
	  L = ?list(Contents, Termination, ?unknown_qual),
	  typer_s:reportError({illegal_list, L})
      end,
      {'List',['maybeimproper'],[]}; %% "maybe_improper_list()";
    _ -> 
      case erl_types:t_is_subtype(erl_types:t_nil(), Termination) of
	true ->
	  {'List',['maybeimproper'],[StringContents,t_to_Statechum(Termination, RecDict)]};
%%,"maybe_improper_list("++ContentString++","++t_to_string(Termination, RecDict)++")";
	false ->
	  {'List',['improper'],[StringContents,t_to_Statechum(Termination, RecDict)]}
%%  "improper_list("++ContentString++","++t_to_string(Termination, RecDict)++")"
      end
  end;
t_to_Statechum(?int_set(Set), _RecDict) ->
	{'Int',['values'],set_to_Statechum(Set)};
t_to_Statechum(?byte, _RecDict) -> {'Byte',[]}; %% "byte()";
t_to_Statechum(?char, _RecDict) -> {'Char',[]}; %% "char()";
t_to_Statechum(?integer_pos, _RecDict) -> {'Int',['positive']}; %% "pos_integer()";
t_to_Statechum(?integer_non_neg, _RecDict) -> {'Int',['nonnegative']}; %% "non_neg_integer()";
t_to_Statechum(?integer_neg, _RecDict) -> {'Int',['negative']}; %% "neg_integer()";
t_to_Statechum(?int_range(From, To), _RecDict) -> {'Int',['boundaries'],[From, To]}; %% OtpErlang will turn list into string but we'll turn it back afterwards
%% but at the same time, it is not proper to turn the last argument from a list into a tuple because we'd like to generate them using 
%% list comprehensions in the union case and others; the trouble with lists integers being autoconverted to strings is relatively minor.

%%  lists:flatten(io_lib:format("~w..~w", [From, To]));
t_to_Statechum(?integer(?any), _RecDict) -> {'Int',[]}; %% "integer()";
t_to_Statechum(?float, _RecDict) -> {'Float',[]}; %% "float()";
t_to_Statechum(?number(?any, ?unknown_qual), _RecDict) -> {'Int',[]}; %% "number()";
t_to_Statechum(?product(_List), _RecDict) -> unsupportedType("product types are not supported");
%% It is not hard to support this type - I could do the same as I did for fun_to_Statechum, 
%% but I do not know when it is used
%% and hence the envelope to use for it.
%%  "<" ++ sequence_to_Statechum(List, RecDict) ++ ">";
t_to_Statechum(?remote(_Set), _RecDict) -> unsupportedType("remote types are not supported");
%%   sequence([case Args =:= [] of
%% 	      true  -> io_lib:format("~w:~w()", [Mod, Name]);
%% 	      false ->
%% 		ArgString = comma_sequence(Args, RecDict),
%% 		io_lib:format("~w:~w(~s)", [Mod, Name, ArgString])
%% 	    end
%% 	    || #remote{mod = Mod, name = Name, args = Args} <- ordsets:to_list(Set)],
%% 	   [], " | ");
t_to_Statechum(?tuple(?any, ?any, ?any), _RecDict) -> {'Tuple',[]}; %% "tuple()";
t_to_Statechum(?tuple(Elements, _Arity, ?any), RecDict) -> {'Tuple',[],sequence_to_Statechum(Elements, RecDict)};
%%  "{" ++ sequence_to_Statechum(Elements, RecDict) ++ "}";
t_to_Statechum(?tuple(Elements, Arity, Tag), RecDict) ->
  [TagAtom] = erl_types:t_atom_vals(Tag),
  case erl_types:lookup_record(TagAtom, Arity-1, RecDict) of
    error -> {'Tuple',[],sequence_to_Statechum(Elements, RecDict)}; %% "{" ++ sequence_to_Statechum(Elements, RecDict) ++ "}";
    {ok, FieldNames} ->
      record_to_Statechum(TagAtom, Elements, FieldNames, RecDict)
  end;
t_to_Statechum(?tuple_set(_) = T, RecDict) ->
  case erl_types:t_tuple_subtypes(T) of
	'unknown' -> typer_s:reportError("set of tuple with arbitrary elements");
	List ->  union_sequence(List, RecDict)
  end;
t_to_Statechum(?union(Types), RecDict) ->
  union_sequence([T || T <- Types, T =/= ?none], RecDict);
t_to_Statechum(?var(Id), _RecDict) when is_atom(Id) -> unsupportedType("variables are not supported");
%%  io_lib:format("~s", [atom_to_list(Id)]);
t_to_Statechum(?var(Id), _RecDict) when is_integer(Id) -> unsupportedType("variables are not supported").
%%  io_lib:format("var(~w)", [Id]).

union_sequence(Types, RecDict) ->
  {'Alt',[],[t_to_Statechum(T, RecDict) || T <- Types]}.

record_to_Statechum(Tag, [_|Fields], FieldNames, RecDict) ->
  FieldStrings = record_fields_to_Statechum(Fields, FieldNames, RecDict, []),
  {'Record',[Tag],FieldStrings}.

record_fields_to_Statechum([F|Fs], [{FName, _DefType}|FDefs], RecDict, Acc) ->
  NewAcc =
    case erl_types:t_is_any(F) orelse erl_types:t_is_atom('undefined', F) of
      true -> Acc;
      false ->
	StrFV = {FName,t_to_Statechum(F, RecDict)},
	%% ActualDefType = t_subtract(DefType, t_atom('undefined')),
	%% Str = case erl_types:t_is_any(ActualDefType) of
	%% 	  true -> StrFV;
	%% 	  false -> StrFV ++ "::" ++ t_to_string(ActualDefType, RecDict)
	%%	end,
	[StrFV|Acc]
    end,
  record_fields_to_Statechum(Fs, FDefs, RecDict, NewAcc);
record_fields_to_Statechum([], [], _RecDict, Acc) ->
  lists:reverse(Acc).

