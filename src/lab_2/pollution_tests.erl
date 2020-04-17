-module(pollution_tests).
-author("Samuel Heldak").

%% API
-export([]).
-include_lib("eunit/include/eunit.hrl").


%% checking if new monitor is empty
createMonitor_test_() -> ?_assert(maps:size(pollution:createMonitor()) =:= 0).


%% checking if addStation function is adding station properly
monitor1() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M2.

%% checking if addStation function generates error in case of duplicating station
monitor1a() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:addStation("Katowice", {1, 1}, M2),
  M3.

monitor1b() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:addStation("Krakow", {2, 2}, M2),
  M3.


%% checking if addValue function is adding value properly
monitor2() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:addValue({1, 1}, {{2020, 04, 02}, {12, 00, 00}}, "PM10", 15, M2),
  M4 = pollution:addValue("Krakow", {{2020, 04, 02}, {12, 00, 00}}, "PM2.5", 30, M3),
  M4.

%% checking if addValue function generates error in case of passing non-existing station
monitor2a() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:addValue({2, 2}, {{2020, 04, 02}, {12, 00, 00}}, "PM10", 15, M2),
  M3.

%% checking if addValue function generates error in case of value duplication
monitor2b() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:addValue({1, 1}, {{2020, 04, 02}, {12, 00, 00}}, "PM10", 15, M2),
  M4 = pollution:addValue("Krakow", {{2020, 04, 02}, {12, 00, 00}}, "PM10", 15, M3),
  M4.


%% checking if removeValue function is removing value properly
monitor3() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:addStation("Katowice", {2, 2}, M2),
  M4 = pollution:addValue({1, 1}, {{2020, 04, 02}, {12, 00, 00}}, "PM10", 15, M3),
  M5 = pollution:addValue({1, 1}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5", 45, M4),
  M6 = pollution:addValue({2, 2}, {{2020, 04, 02}, {13, 00, 00}}, "temp", 3, M5),
  M7 = pollution:removeValue({1, 1}, {{2020, 04, 02}, {12, 00, 00}}, "PM10", M6),
  M8 = pollution:removeValue("Katowice", {{2020, 04, 02}, {13, 00, 00}}, "temp", M7),
  M8.

%% checking if removeValue function generates error in case of passing non-existing station
monitor3a() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:removeValue("Katowice", {{2020, 04, 02}, {13, 00, 00}}, "temp", M2),
  M3.


%% checking if getOneValue function is returning proper value
monitor4() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:addStation("Katowice", {2, 2}, M2),
  M4 = pollution:addValue({1, 1}, {{2020, 04, 02}, {12, 00, 00}}, "PM10", 15, M3),
  M5 = pollution:addValue({1, 1}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5", 45, M4),
  M6 = pollution:addValue({2, 2}, {{2020, 04, 02}, {13, 00, 00}}, "temp", 3, M5),
  M7 = pollution:getOneValue({1, 1}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5", M6),
  M7.

%% checking if getOneValue function generates error in case of passing non-existing station
monitor4a() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:getOneValue({2, 2}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5", M2),
  M3.

%% checking if getOneValue function generates error in case of passing non-existing value
monitor4b() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:addStation("Katowice", {2, 2}, M2),
  M4 = pollution:addValue({1, 1}, {{2020, 04, 02}, {12, 00, 00}}, "PM10", 15, M3),
  M5 = pollution:addValue({1, 1}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5", 45, M4),
  M6 = pollution:addValue({2, 2}, {{2020, 04, 02}, {13, 00, 00}}, "temp", 3, M5),
  M7 = pollution:getOneValue({2, 2}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5", M6),
  M7.


%% checking if getStationMean function is returning proper value
monitor5() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:addStation("Katowice", {2, 2}, M2),
  M4 = pollution:addValue({1, 1}, {{2020, 04, 02}, {12, 00, 00}}, "PM2.5", 15, M3),
  M5 = pollution:addValue({1, 1}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5", 45, M4),
  M6 = pollution:addValue({2, 2}, {{2020, 04, 02}, {13, 00, 00}}, "temp", 3, M5),
  M7 = pollution:getStationMean({1, 1}, "PM2.5", M6),
  M7.

%% checking if getStationMean function generates error in case of passing non-existing station
monitor5a() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:getStationMean("Katowice", "PM2.5", M2),
  M3.


%% checking if getDailyMean function is returning proper value
monitor6() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:addStation("Katowice", {2, 2}, M2),
  M4 = pollution:addValue({1, 1}, {{2020, 04, 02}, {12, 00, 00}}, "PM2.5", 15, M3),
  M5 = pollution:addValue({1, 1}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5", 45, M4),
  M6 = pollution:addValue({2, 2}, {{2020, 04, 02}, {13, 00, 00}}, "PM2.5", 6, M5),
  M7 = pollution:getDailyMean({2020, 04, 02}, "PM2.5", M6),
  M7.


%% checking if getStationWithTheMostMeasurements function is returning proper value
monitor7() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:addStation("Katowice", {2, 2}, M2),
  M4 = pollution:addValue({1, 1}, {{2020, 04, 02}, {12, 00, 00}}, "PM2.5", 15, M3),
  M5 = pollution:addValue({1, 1}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5", 45, M4),
  M6 = pollution:addValue({2, 2}, {{2020, 04, 02}, {13, 00, 00}}, "PM2.5", 4, M5),
  M7 = pollution:addValue({2, 2}, {{2020, 04, 02}, {18, 00, 00}}, "PM2.5", 8, M6),
  M8 = pollution:addValue({2, 2}, {{2020, 04, 02}, {19, 00, 00}}, "PM2.5", 16, M7),
  M9 = pollution:getStationWithTheMostMeasurements(M8),
  M9.

%% checking if getStationWithTheMostMeasurements function generates error in case of no stations
monitor7a() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:getStationWithTheMostMeasurements(M1),
  M2.


%% checking if getNearestStation function is returning proper coordinates
monitor8({X, Y}) ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:addStation("Katowice", {1, 3}, M2),
  M4 = pollution:addStation("Warszawa", {3, 2}, M3),
  M5 = pollution:addStation("Poznan", {3, 3}, M4),
  M6 = pollution:getNearestStation({X, Y}, M5),
  M6.

%% checking if getNearestStation function generates error in case of just one station or
%% passing non-existing station
monitor8a({X, Y}) ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:getNearestStation({X, Y}, M2),
  M3.


integration_test_() ->
  [ ?_assert(monitor1() =:= #{{1, 1} => #{name => "Krakow"}}),
    ?_assertException(error, duplicated_station, monitor1a()),
    ?_assertException(error, duplicated_station, monitor1b()),

    ?_assert(monitor2() =:= #{{1, 1} =>
    #{name => "Krakow",
      {{{2020, 04, 02}, {12, 00, 00}}, "PM10"} => 15,
      {{{2020, 04, 02}, {12, 00, 00}}, "PM2.5"} => 30}}),
    ?_assertException(error, no_station, monitor2a()),
    ?_assertException(error, duplicated_value, monitor2b()),

    ?_assert(monitor3() =:=
      #{{1, 1} =>
        #{name => "Krakow",
        {{{2020, 04, 02}, {16, 00, 00}}, "PM2.5"} => 45},
        {2, 2} =>
        #{name => "Katowice"}}),
    ?_assertException(error, no_station, monitor3a()),

    ?_assert(monitor4() =:= 45),
    ?_assertException(error, no_station, monitor4a()),
    ?_assertException(error, no_value, monitor4b()),

    ?_assert(monitor5() == 30),
    ?_assertException(error, no_station, monitor5a()),

    ?_assert(monitor6() == 22),

    ?_assert(monitor7() =:= {2, 2}),
    ?_assertException(error, empty_monitor, monitor7a()),

    ?_assert(monitor8({1, 1}) =:= {1, 3}),
    ?_assert(monitor8({3, 2}) =:= {3, 3}),
    ?_assert(monitor8({3, 3}) =:= {3, 2}),
    ?_assertException(error, just_one_station, monitor8a({1, 1})),
    ?_assertException(error, no_station, monitor8a({2, 2}))
  ].