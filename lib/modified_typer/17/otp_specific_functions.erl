constructStrippedCG0(StrippedCG0)->
	StrippedCG0.

extractLineNoAndFileName(FunBody)->
  Anno = cerl:get_ann(FunBody),
  {get_line(Anno),get_file(Anno)}.

% Verbatim from Typer in Otp18.0
get_line([Line|_]) when is_integer(Line) -> Line;
get_line([_|T]) -> get_line(T);
get_line([]) -> none.

% Verbatim from Typer in Otp18.0
get_file([{file,File}|_]) -> File;
get_file([_|T]) -> get_file(T);
get_file([]) -> "no_file". % should not happen
