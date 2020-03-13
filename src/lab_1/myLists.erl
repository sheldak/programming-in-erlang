%%%-------------------------------------------------------------------
%%% @author sheldak
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Mar 2020 12:14
%%%-------------------------------------------------------------------
-module(myLists).
-author("sheldak").

%% API
-export([contains/2, duplicateElements/1, sumFloats/1]).


contains([Value | T], Value) -> true;
contains([_ | T], Value)     -> contains(T, Value);
contains([], _)              -> false.

duplicateElements([H | T]) -> [H, H | duplicateElements(T)];
duplicateElements([])      -> [].

sumFloatsRec([H | T], Acc) -> sumFloatsRec(T, Acc + H);
sumFloatsRec([], Acc)      -> Acc.

sumFloats(List) -> sumFloatsRec(List, 0.0).