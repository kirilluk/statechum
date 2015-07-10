constructStrippedCG0(StrippedCG0)->
	dialyzer_callgraph:finalize(StrippedCG0).
	
	
extractLineNoAndFileName(FunBody)->
	[_, LineNo, {file, FileName}] = cerl:get_ann(FunBody),
	{LineNo,FileName}.
