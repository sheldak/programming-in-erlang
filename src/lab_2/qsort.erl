%%%-------------------------------------------------------------------
%%% @author sheldak
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Mar 2020 20:07
%%%-------------------------------------------------------------------
-module(qsort).
-author("Samuel Heldak").

%% API
-export([qs/1, randomElems/3, compareSpeeds/3]).

lessThan(List, Arg) -> [X || X <- List, X < Arg].

grtEqThan(List, Arg) -> [X || X <- List, X >= Arg].

qs([])             -> [];
qs([Pivot | Tail]) -> qs( lessThan(Tail, Pivot) ) ++ [Pivot] ++ qs( grtEqThan(Tail, Pivot) ).


randomElems(N, Min, Max) -> [(Max - Min) * rand:uniform() + Min || _ <- lists:seq(1, N)].


compareSpeeds(List, Fun1, Fun2) ->
  {T1, _V1} = timer:tc(Fun1, [List]),
  {T2, _V2} = timer:tc(Fun2, [List]),
  io:format("First function:  ~B~nSecond function: ~B~n", [T1, T2]).
