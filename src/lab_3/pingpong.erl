%%%-------------------------------------------------------------------
%%% @author sheldak
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Apr 2020 10:32
%%%-------------------------------------------------------------------
-module(pingpong).
-author("sheldak").

%% API
-export([start/0, init/0, stop/0, play/1]).

start() ->
  PidPing = spawn(?MODULE, init, []),
  PidPong = spawn(?MODULE, init, []),
  register(ping, PidPing),
  register(pong, PidPong).

init() ->
  loop(0).

loop(Sum) ->
  receive
    {play, N} ->
      io:format("Starting ping pong, ~B messages ~nSum of communications: ~B~n", [N, Sum+N]),
      timer:sleep(50),
      pong ! {pong, N-1},
      loop(Sum + N);
    {ping, 0} ->
      io:format("PING ~nping pong ended ~nSum of communications: ~B~n~n", [Sum]),
      loop(Sum);
    {ping, N} ->
      io:format("PING, ~B left ~nSum of communications: ~B~n", [N-1, Sum]),
      timer:sleep(50),
      pong ! {pong, N-1},
      loop(Sum);
    {pong, 0} ->
      io:format("PONG ~nping pong ended~n~n"),
      loop(Sum);
    {pong, N} ->
      io:format("PONG, ~B left~n", [N-1]),
      timer:sleep(150),
      ping ! {ping, N-1},
      loop(Sum);
    stop      -> terminate()
  after
    20000 -> terminate()
  end.

terminate() ->
  ok.

play(N) ->
  ping ! {play, N}.

stop() ->
  ping ! stop,
  pong ! stop.