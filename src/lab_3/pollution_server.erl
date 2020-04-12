%%%-------------------------------------------------------------------
%%% @author Samuel Heldak
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Apr 2020 14:54
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("Samuel Heldak").

%% API
-export([start/0, init/0, stop/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2,
  getDailyMean/2, getStationWithTheMostMeasurements/0, getNearestStation/1]).
-include_lib("eunit/include/eunit.hrl").

%% --- Server ---
%% starting the server
start() ->
  MonitorPid = spawn(?MODULE, init, []),
  register(monitor, MonitorPid).


%% initializing process loop
init() ->
  Monitor = pollution:createMonitor(),
  loop(Monitor).

%% auxiliary function to sending messages and returning to the loop
sendMessage(Message, Destination, Monitor) ->
  Destination ! Message,
  loop(Monitor).

%% reacting on messages
loop(Monitor) ->
  receive
    {addStation, Name, Coordinates, Caller} ->
      try pollution:addStation(Name, Coordinates, Monitor) of
        NewMonitor -> sendMessage(ok, Caller, NewMonitor)
      catch
        error:Message -> sendMessage({error, Message}, Caller, Monitor)
      end;
    {addValue, Name, Date, Type, Value, Caller} ->
      try pollution:addValue(Name, Date, Type, Value, Monitor) of
        NewMonitor -> sendMessage(ok, Caller, NewMonitor)
      catch
        error:Message -> sendMessage({error, Message}, Caller, Monitor)
      end;
    {removeValue, Name, Date, Type, Caller} ->
      try pollution:removeValue(Name, Date, Type, Monitor) of
        NewMonitor -> sendMessage(ok, Caller, NewMonitor)
      catch
        error:Message -> sendMessage({error, Message}, Caller, Monitor)
      end;
    {getOneValue, Name, Date, Type, Caller} ->
      try pollution:getOneValue(Name, Date, Type, Monitor) of
        Value -> sendMessage({ok, Value}, Caller, Monitor)
      catch
        error:Message -> sendMessage({error, Message}, Caller, Monitor)
      end;
    {getStationMean, Name, Type, Caller} ->
      try pollution:getStationMean(Name, Type, Monitor) of
        Mean -> sendMessage({ok, Mean}, Caller, Monitor)
      catch
        error:Message -> sendMessage({error, Message}, Caller, Monitor)
      end;
    {getDailyMean, Day, Type, Caller} ->
      Mean = pollution:getDailyMean(Day, Type, Monitor),
      sendMessage({ok, Mean}, Caller, Monitor);
    {getStationWithTheMostMeasurements, Caller} ->
      try pollution:getStationWithTheMostMeasurements(Monitor) of
        {X, Y} -> sendMessage({ok, {X, Y}}, Caller, Monitor)
      catch
        error:Message -> sendMessage({error, Message}, Caller, Monitor)
      end;
    {getNearestStation, Coordinates, Caller} ->
      try pollution:getNearestStation(Coordinates, Monitor) of
        {X, Y} -> sendMessage({ok, {X, Y}}, Caller, Monitor)
      catch
        error:Message -> sendMessage({error, Message}, Caller, Monitor)
      end;
    stop      -> terminate()
  after
    20000 -> terminate()
  end.

%% ending process running
terminate() ->
  ok.

%% terminating the process
stop() ->
  monitor ! stop.


%% --- Functions to manage the monitor ---
%% auxiliary function to printing preformed action and returning "ok", error or some value
printAndReturn(Text, Args, Return) ->
  io:format(Text, Args),
  Return.


%% adding new station
addStation(Name, Coordinates) ->
  monitor ! {addStation, Name, Coordinates, self()},
  receive
    ok ->
      printAndReturn("Station ~s added.~n", [Name], ok);
    {error, duplicated_station} ->
      printAndReturn("Cannot add station. Duplicated station's name or coordinates.~n", [], {error, duplicated_station})
  end.


%% adding new measurement to the station
addValue(Name, Date, Type, Value) ->
  monitor ! {addValue, Name, Date, Type, Value, self()},
  receive
    ok ->
      printAndReturn("Measurement ~s = ~B added to the station.~n", [Type, Value], ok);
    {error, no_station} ->
      printAndReturn("Cannot add value. Cannot find station with that name.~n", [], {error, no_station});
    {error, duplicated_value} ->
      printAndReturn("Cannot add value. Duplicated measurement.~n", [], {error, duplicated_value})
  end.


%% removing a measurement
removeValue(Name, Date, Type) ->
  monitor ! {removeValue, Name, Date, Type, self()},
  receive
    ok ->
      printAndReturn("Measurement removed from the station.~n", [], ok);
    {error, no_station} ->
      printAndReturn("Cannot remove value. Cannot find station with that name.~n", [], {error, no_station})
  end.


%% getting value of the measurement
getOneValue(Name, Date, Type) ->
  monitor ! {getOneValue, Name, Date, Type, self()},
  receive
    {ok, Value} ->
      printAndReturn("The result of the measurement is ~w.~n", [Value], {ok, Value});
    {error, no_station} ->
      printAndReturn("Cannot get value. Cannot find station with that name.~n", [], {error, no_station});
    {error, no_value} ->
      printAndReturn("Cannot get value. There is no such measurement.~n", [], {error, no_value})
  end.


%% getting mean value of all the measurements at the station
getStationMean(Name, Type) ->
  monitor ! {getStationMean, Name, Type, self()},
  receive
    {ok, Mean} ->
      printAndReturn("Station mean is equal to ~w.~n", [Mean], {ok, Mean});
    {error, no_station} ->
      printAndReturn("Cannot get the mean. Cannot find station with that name.~n", [], {error, no_station})
  end.


%% getting daily mean value of measurements of the given Type from all stations
getDailyMean(Day, Type) ->
  monitor ! {getDailyMean, Day, Type, self()},
  receive
    {ok, Mean} ->
      printAndReturn("Daily mean is equal to ~w.~n", [Mean], {ok, Mean})
  end.


%% getting coordinates ({X, Y}) of the station which has the highest number of measurements
getStationWithTheMostMeasurements() ->
  monitor ! {getStationWithTheMostMeasurements, self()},
  receive
    {ok, {X, Y}} ->
      printAndReturn("The most measurements has the station (~w,~w).~n", [X, Y], {ok, {X, Y}});
    {error, empty_monitor} ->
      printAndReturn("Cannot get the station. Empty monitor.~n", [], {error, empty_monitor})
  end.


%% function returning coordinates of the station which are the closest to the given one
getNearestStation(Coordinates) ->
  monitor ! {getNearestStation, Coordinates, self()},
  receive
    {ok, {X, Y}} ->
      printAndReturn("The closest station is (~w,~w).~n", [X, Y], {ok, {X, Y}});
    {error, no_station} ->
      printAndReturn("There is no station with such coordinates.~n", [], {error, no_station});
    {error, just_one_station} ->
      printAndReturn("Cannot find the closest station. There is just one in the monitor.~n", [], {error, just_one_station})
  end.