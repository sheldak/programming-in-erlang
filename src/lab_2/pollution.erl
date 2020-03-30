%%%-------------------------------------------------------------------
%%% @author sheldak
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Mar 2020 18:26
%%%-------------------------------------------------------------------
-module(pollution).
-author("sheldak").

%% API
-export([createMonitor/0, addStation/3, addValue/5, listLen/1, isStation/2]).

createMonitor() -> #{}.


addStation(Name, {X, Y}, Monitor) ->
  Key = {X, Y},
  case maps:find({X, Y}, Monitor) of
    {ok, _} -> {error, duplicated_station};
    _ -> Monitor#{Key => #{name => Name}}
  end.


listLen([]) -> 0;
listLen([_ | T]) -> 1 + listLen(T).


isStation(Name, Monitor) ->
  Pred = fun(M) -> listLen(lists:filter(fun(V) -> V == Name end, maps:values(M))) == 1 end,
  ListWithStation = lists:filter(Pred, maps:values(Monitor)),

  case listLen(ListWithStation) of
    0 -> false;
    _  -> true
  end.


findKey(Name, Monitor) ->
  Pred = fun({_, M}) -> listLen(lists:filter(fun(V) -> V == Name end, maps:values(M))) == 1 end,
  [{K, _}] = lists:filter(Pred, maps:to_list(Monitor)),

  K.

% TODO check if value has already been added before to avoid duplications
addValue({X, Y}, Date, Type, Value, Monitor) ->
  Key = {X, Y},
  #{Key := PrevVal} = Monitor,
  case maps:is_key({X, Y}, Monitor) of
    true  -> Monitor#{{X, Y} := PrevVal#{{Date, Type} => Value}};
    false -> {error, no_station}
  end;
addValue(Name, Date, Type, Value, Monitor) ->
  case isStation(Name, Monitor) of
    false -> {error, no_station};
    _     -> addValue(findKey(Name, Monitor), Date, Type, Value, Monitor)
  end.
