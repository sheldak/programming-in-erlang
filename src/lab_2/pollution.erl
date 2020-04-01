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
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3]).

createMonitor() -> #{}.


listLen([]) -> 0;
listLen([_ | T]) -> 1 + listLen(T).


isStation(Name, Monitor) ->
  Pred = fun(M) -> listLen(lists:filter(fun(V) -> V == Name end, maps:values(M))) == 1 end,
  ListWithStation = lists:filter(Pred, maps:values(Monitor)),

  case listLen(ListWithStation) of
    0 -> false;
    _ -> true
  end.


addStation(Name, {X, Y}, Monitor) ->
  case maps:is_key({X, Y}, Monitor) or isStation(Name, Monitor) of
    true -> error(duplicated_station);
    _    -> Monitor#{{X, Y} => #{name => Name}}
  end.


findKey(Name, Monitor) ->
  Pred = fun({_, M}) -> listLen(lists:filter(fun(V) -> V == Name end, maps:values(M))) == 1 end,
  [{K, _}] = lists:filter(Pred, maps:to_list(Monitor)),

  K.


addValue({X, Y}, Date, Type, Value, Monitor) ->
  case maps:is_key({X, Y}, Monitor) of
    true -> StationValues = maps:get({X, Y}, Monitor),

            case maps:is_key({Date, Type}, StationValues) of
              true -> error(duplicated_value);
              _    -> Monitor#{{X, Y} => maps:put({Date, Type}, Value, StationValues)}
            end;
    _    -> error(no_station)
  end;

addValue(Name, Date, Type, Value, Monitor) ->
  case isStation(Name, Monitor) of
    true -> addValue(findKey(Name, Monitor), Date, Type, Value, Monitor);
    _    -> error(no_station)
  end.


removeValue({X, Y}, Date, Type, Monitor) ->
  case maps:is_key({X, Y}, Monitor) of
    true -> StationValues = maps:get({X, Y}, Monitor),
            Monitor#{{X, Y} => maps:remove({Date, Type}, StationValues)};
    _    -> error(no_station)
  end;

removeValue(Name, Date, Type, Monitor) ->
  case isStation(Name, Monitor) of
    true -> removeValue(findKey(Name, Monitor), Date, Type, Monitor);
    _    -> error(no_station)
  end.


getOneValue({X, Y}, Date, Type, Monitor) ->
  case maps:is_key({X, Y}, Monitor) of
    true -> StationValues = maps:get({X, Y}, Monitor),

            case maps:is_key({Date, Type}, StationValues) of
              true -> maps:get({Date, Type}, StationValues);
              _    -> error(no_value)
            end;
    _    -> error(no_station)
  end;

getOneValue(Name, Date, Type, Monitor) ->
  case isStation(Name, Monitor) of
    true -> getOneValue(findKey(Name, Monitor), Date, Type, Monitor);
    _    -> error(no_station)
  end.


getStationMean({X, Y}, Type, Monitor) ->
  case maps:is_key({X, Y}, Monitor) of
    true -> StationValues = maps:get({X, Y}, Monitor),
            StationMeasurements = maps:filter(fun({_D, T}, _V) -> T == Type; (_, _) -> false end, StationValues),
            MeasurementsNumber = max(maps:size(StationMeasurements), 1),
            MeasurementsSum = maps:fold(fun(_K, V, Acc) -> V + Acc end, 0, StationMeasurements),
            MeasurementsSum / MeasurementsNumber;
    _    -> error(no_station)
  end;

getStationMean(Name, Type, Monitor) ->
  case isStation(Name, Monitor) of
    true -> getStationMean(findKey(Name, Monitor), Type, Monitor);
    _    -> error(no_station)
  end.


getDailyMean(Day, Type, Monitor) ->
  DayTypeFilterPred = fun ({{D, _H}, T}, _V) -> X = T == Type,
                                                Y = D == Day,
                                                X and Y;
                            (_, _)           -> false
                      end,
  FilteredMonitor = maps:map(fun(_K, V) -> maps:filter(DayTypeFilterPred, V) end, Monitor),

  FoldStationFunc = fun(_K, V, Acc) -> Acc + V end,
  DailySum = maps:fold(fun(_K, V, Acc) -> Acc + maps:fold(FoldStationFunc, 0, V) end, 0, FilteredMonitor),
  DailySize = max(maps:fold(fun(_K, V, Acc) -> Acc + maps:size(V) end, 0, FilteredMonitor), 1),

  DailySum / DailySize.