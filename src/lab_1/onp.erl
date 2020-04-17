-module(onp).
-author("sheldak").

%% API
-export([onp/1]).


check_int({H, []}, T, Stack)  -> process(T, [H | Stack]);
check_int({_, [_ | _]}, _, _) -> error(invalid_input).

opposite(Number) -> -Number.

process("", []) -> 0;
process("", [H]) -> H;
process(["+" | T], [SH, SHH | ST])    -> process(T, [(SHH + SH) | ST]);
process(["-" | T], [SH, SHH | ST])    -> process(T, [(SHH - SH) | ST]);
process(["*" | T], [SH, SHH | ST])    -> process(T, [(SHH * SH) | ST]);
process(["/" | T], [SH, SHH | ST])    -> process(T, [(SHH / SH) | ST]);
process(["^" | T], [SH, SHH | ST])    -> process(T, [math:pow(SHH, SH) | ST]);
process(["mean" | T], [SH, SHH | ST]) -> process(T, [(SHH + SH) / 2 | ST]);
process(["sqrt" | T], [SH | ST])      -> process(T, [math:sqrt(SH) | ST]);
process(["sin" | T], [SH | ST])       -> process(T, [math:sin(SH) | ST]);
process(["cos" | T], [SH | ST])       -> process(T, [math:cos(SH) | ST]);
process(["tan" | T], [SH | ST])       -> process(T, [math:tan(SH) | ST]);
process(["opp" | T], [SH | ST])       -> process(T, [opposite(SH) | ST]);
process([H | T], Stack)               -> check_int(string:to_integer(H), T, Stack).

onp(Statement) -> process(string:tokens(Statement, " "), []).
