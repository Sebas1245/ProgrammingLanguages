-module(fact).
-export([factorial/1, conditional_factorial/1]).

factorial (0) -> 1;
factorial (N) when N > 0 -> 
    N * factorial(N-1).


conditional_factorial (N) -> 
    if
        N =:= 0 -> 
            1;
        true -> % equivalent to else
            N * conditional_factorial(N-1)
    end.