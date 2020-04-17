-module(higherOrder).
-author("sheldak").

%% API
-export([map/2, filter/2, sum_of_digits/1, get_divisible/1]).

map(Fun, List) -> [Fun(X) || X <- List].

filter(Fun, List) -> [X || X <- List, Fun(X)].


split(0)      -> [];
split(Number) ->  split(Number div 10) ++ [Number rem 10].

sum_of_digits(Number) -> lists:foldl(fun (X, Y) -> X + Y end, 0, split(Number)).


get_divisible(List) -> lists:filter(fun (X) -> sum_of_digits(X) rem 3 == 0 end, List).
