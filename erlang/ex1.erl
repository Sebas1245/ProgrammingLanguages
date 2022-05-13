-module(ex1).
-import(math, [sqrt/1]).
-export([general_formula/3, sequence/1, maps/2]).

% uses the general formula to get the roots of a quadratic equation
general_formula(A, B, C) ->
    Discr = ((B*B)-(4*A*C)),
    if  Discr < 0 -> imaginary;
    true -> 
        {(-B+sqrt(Discr))/(2*A), (-B-sqrt(Discr))/(2*A)}
    end.

% creates a list for the sequence [1..N]
sequence(0) -> [];
sequence(N) when N > 0 -> sequence(N-1) ++ [N].

% function that applies a function to every element of a list to produce a new list
maps(_, []) -> [];
maps(Func, [First | Rest]) -> [Func(First) | maps(Func, Rest)].