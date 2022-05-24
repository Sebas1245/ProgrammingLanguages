-module(compute).

compute() -> compute(0, 0).
compute(L, S) -> 
    receive
        {sum, X, Y} ->
            L = X + Y,
            S = S + L,
            L;
        {multiply, X, Y} -> 
            L = X * Y,
            S = S + L,
            L;
        {last, P} -> P ! {result, L};
        {total, P} -> P ! {result, S}
    end.