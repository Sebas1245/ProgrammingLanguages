% Assignment 7
% Author: Sebastián Saldaña

% Excercise 1
divisible(X,Y) :- 0 is X mod Y, !.

divisible(X,Y) :- X > Y+1, divisible(X, Y+1).

prime(2) :- true,!.
prime(X) :- X < 2,!,false.
prime(X) :- not(divisible(X, 2)).

% Excercise 2
range(N,N,[N]) :- !.
range(N,M,[N|R]) :-
    N =< M,
    K is N+1,
    range(K,M,R).

% Excercise 3
cartesian([],_,[]).
cartesian(_,[],[]).
cartesian([H1|T1],L2,R):- 
    cartesian_helper(H1,L2,R1),
    cartesian(T1,L2,R2),
    append(R1,R2,R).

cartesian_helper(_,[],[]).
cartesian_helper(X,[H|T],[[X,H]|R]):- 
    cartesian_helper(X,T,R).

% Excercise 4
count_deep(_, [], 0).
count_deep(E, [E | T], R) :-
    !,
    count_deep(E, T, N),
    R is N + 1.
count_deep(E, [D | T], R) :-
    atom(D),
    count_deep(E, T, R),
    !.
count_deep(E, [L | T], R) :-
    count_deep(E, L, M),
    count_deep(E, T, K),
    R is M + K.

% Excercise 5
unique_list([], []) :- !.
unique_list([F | T], R) :-
    \+ member(F, T),
    unique_list(T, L),
    R is append(L, [F], R).