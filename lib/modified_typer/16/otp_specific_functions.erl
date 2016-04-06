constructStrippedCG0(StrippedCG0)->
	StrippedCG0.
	
	
extractLineNoAndFileName(FunBody)->
	[_, LineNo, {file, FileName}] = cerl:get_ann(FunBody),
	{LineNo,FileName}.
