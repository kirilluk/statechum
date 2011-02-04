-module(tracer_coverage).
-export([cover_map/3,cover_map/4,map_intersect/2,map_exclude/2,cover_map_to_file/5]).

cover_map_to_file(Module, Function, Prefix, Suffix, FileName) ->
    {_Status, Map} = cover_map_html(Module, Function, Prefix, Suffix, FileName),
    {ok, IODevice} = file:open(FileName, [write]),
    io:format(IODevice, "~p", [Map]),
    file:close(IODevice).

cover_map_html(Module, Function, [], Suffix, FileName) ->
    cover:compile(Module),
    {Pid, Ref} = spawn_monitor(Module, Function, [Suffix]),
    ProcStatus = tracer:await_end(Pid, Ref),
    demonitor(Ref),
    cover:analyse_to_file(Module, FileName, []),
    %%cover:analyse_to_file(Module, FileName ++ ".html", [html]),
    {ProcStatus, create_map(FileName)};

cover_map_html(Module, Function, Prefix, Suffix, FileName) ->
    {PStatus, PrefixMap} = cover_map_html(Module, Function, Prefix, FileName),
    case PStatus of
	ok ->
	    {FStatus, FullMap} = cover_map_html(Module, Function, Prefix ++ Suffix, FileName),
	    {FStatus, map_subtract(PrefixMap, FullMap)};
	failed ->
	    {PStatus, []}
    end.

cover_map_html(Module, Function, Suffix, FileName) ->
    cover_map_html(Module, Function, [], Suffix, FileName).


cover_map(Module, Function, [], Suffix) ->
    cover:compile(Module),
    {Pid, Ref} = spawn_monitor(Module, Function, [Suffix]),
    ProcStatus = tracer:await_end(Pid, Ref),
    demonitor(Ref),
    FileName = atom_to_list(Module) ++ "-" ++ atom_to_list(Function) ++ ".cover",
    cover:analyse_to_file(Module, FileName, []),
    {ProcStatus, create_map(FileName)};

cover_map(Module, Function, Prefix, Suffix) ->
    {PStatus, PrefixMap} = cover_map(Module, Function, Prefix),
    case PStatus of
	ok ->
	    {FStatus, FullMap} = cover_map(Module, Function, Prefix ++ Suffix),
	    {FStatus, map_subtract(PrefixMap, FullMap)};
	failed ->
	    {PStatus, []}
    end.

cover_map(Module, Function, Suffix) ->
    cover_map(Module, Function, [], Suffix).

%%
%% Helper functions
%%

create_map(FileName) ->
    {ok, IODevice} = file:open(FileName, [read]),
    %% Four lines of headers...
    map_lines(IODevice, [], -3).

map_lines(IODevice, Accum, LineNum) ->
    case io:get_line(IODevice, "") of
        eof  -> file:close(IODevice), Accum;
        Line -> NewAccum = process_line(Line, Accum, LineNum),
                    map_lines(IODevice, NewAccum, LineNum+1)
    end.

process_line(Line, Accum, LineNum) ->
%%    io:format("~p: ~p~n", [LineNum, Line]),
    Toks = string:tokens(Line, ".."),
    Z = string:strip(hd(Toks)),
    case string:to_integer(Z) of
	{error, _Reason} ->
	    Accum;
	{Int, _Rest} ->
	    if (Int > 0) ->
		    Accum ++ [{LineNum, Int}];
	       true ->
		    Accum
	    end
    end.

map_subtract([], FullMap) ->
    FullMap;
map_subtract([{Line, Count} | PrefixMap], FullMap) ->
    NewFullMap = lists:map(fun({ELine, ECount}) -> 
				   if (ELine == Line) ->
					   {ELine, (ECount - Count)};
				      true ->
					   {ELine, ECount}
				   end
			   end, 
			   FullMap),
    map_subtract(PrefixMap, lists:filter(fun({_ELine, ECount}) -> ECount > 0 end, NewFullMap)).

map_intersect([], _) ->
    [];
map_intersect(_, []) ->
    [];
map_intersect([{Line, Count} |Map1], Map2) ->
    case lists:keyfind(Line, 1, Map2) of
	{Line, Count2} ->
	    [{Line, Count, Count2} | map_intersect(Map1, Map2)]; 
	false ->
	    map_intersect(Map1, Map2)
    end.

map_exclude([], M2) ->
    M2;
map_exclude(_, []) ->
    [];
map_exclude([{Line, _Count} | M1], M2) ->
    map_exclude(M1, lists:filter(fun({L, _C}) -> L /= Line end, M2)).
