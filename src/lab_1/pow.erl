-module(pow).
-author("sheldak").

%% API
-export([power/2]).


power(Basis, Exponent) when is_integer(Exponent) and (Exponent > 1) -> Basis * power(Basis, Exponent-1);
power(Basis, 1)                                                     -> Basis;
power(Basis, 0)                                                     -> 1.
