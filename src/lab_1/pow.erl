%%%-------------------------------------------------------------------
%%% @author sheldak
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Mar 2020 09:55
%%%-------------------------------------------------------------------
-module(pow).
-author("sheldak").

%% API
-export([power/2]).


power(Basis, Exponent) when is_integer(Exponent) and (Exponent > 1) -> Basis * power(Basis, Exponent-1);
power(Basis, 1)                                                     -> Basis;
power(Basis, 0)                                                     -> 1.
