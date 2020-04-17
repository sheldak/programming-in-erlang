-module(parcellockerfinder).
-author("sheldak").

%% API
-export([getRandomPlaces/1, findMyParcelLocker/2, findSequentially/2,
  findVeryDistributed/2, findMyParcelLockerDistributed/3,
  findSlightlyDistributed/3, findMyParcelLockerSlightlyDistributed/3, compareSpeeds/2]).


%% getting N random places in 2d area [0, 10000]x[0, 10000]
getRandomPlaces(N) ->
  [{rand:uniform(10001)-1, rand:uniform(10001)-1} || _ <- lists:seq(1, N)].


%% --- Sequential ---
%% returning coordinates which are closer to {X, Y} in Euclidean metric
closer({X, Y}, {X1, Y1}, {X2, Y2})
  when ((X - X1)*(X - X1) + (Y - Y1)*(Y - Y1)) =< ((X - X2)*(X - X2) + (Y - Y2)*(Y - Y2)) ->
  {X1, Y1};
closer ({_X, _Y}, {_X1, _Y1}, {X2, Y2}) ->
  {X2, Y2}.

%% finding the closest locker for one person
findMyParcelLocker(PersonLocation, LockerLocations) ->
  FoldFun = fun (Locker1, Locker2) -> closer(PersonLocation, Locker1, Locker2) end,
  lists:foldl(FoldFun, lists:last(LockerLocations), LockerLocations).


%% finding the closest lockers for list of People
%% returning pair: {person's coordinates, the closest locker's coordinates}
findSequentially(PersonLocations, LockerLocations) ->
  [{Person, findMyParcelLocker(Person, LockerLocations)} || Person <- PersonLocations].


%% --- Very Distributed ---
%% sending person and theirs closest locker to the parent process
findMyParcelLockerDistributed(PersonLocation, LockerLocations, ParentPID) ->
  ParentPID ! {PersonLocation, findMyParcelLocker(PersonLocation, LockerLocations)}.


%% waiting for all pairs: {person, locker} and returning list of such pairs
receivePairs(Pairs, Num, AllProcesses) when Num == AllProcesses ->
  Pairs;
receivePairs(Pairs, Num, AllProcesses) ->
  receive
    Pair -> receivePairs([Pair | Pairs], Num+1, AllProcesses)
  end.

%% Making a process for every person to find their locker
spawnProcesses([], _LockerLocations, AllProcesses) ->
  receivePairs([], 0, AllProcesses);
spawnProcesses([FirstPerson | PersonLocations], LockerLocations, AllProcesses) ->
  spawn(?MODULE, findMyParcelLockerDistributed, [FirstPerson,LockerLocations, self()]),
  spawnProcesses(PersonLocations, LockerLocations, AllProcesses).

%% finding the closest locker for every person by using number of processes equal to number of people
findVeryDistributed(PersonLocations, LockerLocations) ->
  spawnProcesses(PersonLocations, LockerLocations, length(PersonLocations)).


%% --- Slightly Distributed ---
%% finding the closest locker for a list of people
findMyParcelLockerSlightlyDistributed(PersonLocations, LockerLocations, ParentPID) ->
  lists:map(fun (Person) -> findMyParcelLockerDistributed(Person, LockerLocations, ParentPID) end, PersonLocations).

%% dividing all people to All groups where All is the number of cores in my processor (8)
%% and making a process for every group
divideAndSpawn(PersonLocations, _LockerLocations, Start, All) when Start == All ->
  receivePairs([], 0, length(PersonLocations));
divideAndSpawn(PersonLocations, LockerLocations, Start, All) ->
  PeoplePart = lists:sublist(PersonLocations, trunc(Start*length(PersonLocations)/All) + 1,
    trunc((Start+1)*length(PersonLocations)/All) - trunc(Start*length(PersonLocations)/All)),

  spawn(?MODULE, findMyParcelLockerSlightlyDistributed, [PeoplePart, LockerLocations, self()]),
  divideAndSpawn(PersonLocations, LockerLocations, Start+1, All).

%% finding people's closest lockers by using number of processes equal to number of my CPU cores (8)
findSlightlyDistributed(PersonLocations, LockerLocations, Cores) ->
  divideAndSpawn(PersonLocations, LockerLocations, 0, Cores).


%% --- Comparing time of running for all three approaches ---
compareSpeeds(People, Lockers) ->
  {T1, _V1} = timer:tc(fun findSequentially/2, [People, Lockers]),
  {T2, _V2} = timer:tc(fun findVeryDistributed/2, [People, Lockers]),
  {T3, _V3} = timer:tc(fun findSlightlyDistributed/3, [People, Lockers, 8]),
  io:format("Sequentially: ~B~nVery distributed: ~B~nSlightly Distributed: ~B~n", [T1, T2, T3]).