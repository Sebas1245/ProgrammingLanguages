% Solutions to Exercise 17 problems
% Author: Dr. Santiago Conant

% (2) displays the sequence N,N-1,N-2,…,1 with line 
%     breaks after each number.
sequence(N) :-
    N > 0,
    N1 is N - 1,
    sequence(N1),
    write(N), nl.
sequence(0).

% (3) displays multiples of N less than or equal to M 
%     (including N) by separating each multiple with 
%     a space.
multiples(N, M) :- multiples(N, N, M).

multiples(N, Mult, M) :-
    Mult =< M,
    write(Mult), write(" "),
    NMult is Mult + N,
    multiples(N, NMult, M).
multiples(_, Mult, M) :- Mult > M, nl.

% (4) returns the number of digits that a 
%     non-negative integer has.
digits(N, D) :- 
    N > 9,
    NN is N // 10,
    digits(NN, ND),
    D is ND + 1.
digits(N, 1) :- N < 10.
