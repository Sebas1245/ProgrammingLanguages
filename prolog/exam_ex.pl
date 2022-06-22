% Ex 1
divide(L, N, [], L) :- N =< 0, !.
divide([H | T], N, [H |L1], L2) :-
    M is N - 1,
    divide(T, M, L1, L2).

% Ex 2
groups([], []).     
groups([X], [[X]]).   
groups([X,X|T], [[X|PH]|PT]):-
    groups([X|T], [PH|PT]).
groups([X,Y|T], [[X]|PT]):-   
    X \= Y,
    groups([Y|T], PT).


sums(tree(_, nil, nil), nil) :- !.
sums(tree(Node, tree(LC, L1, R1), tree(RC, L2, R2)), 
    tree(Node, tree(SumNode1, tree(LC, L1, R1)), tree(SumNode2, tree(LC, L1, R1)))) :-
        sums(tree(LC, L1, R1), N),
        sums(tree(RC, L2, R2), N),
        SumNode1 is Node + LC,
        SumNode2 is Node + RC.


sum_leaves(Tree, Sum) :-
    sum_leaves(Tree, 0, Sum).

sum_leaves(nil, Sum, Sum).
sum_leaves(tree(Value,nil,nil), Sum0, Sum) :-
    !,
    Sum is Sum0 + Value. 
sum_leaves(tree(_,Left,Right), Sum0, Sum) :-
    sum_leaves(Left, Sum0, Sum1),
    sum_leaves(Right, Sum1, Sum).


exam(N, 1) :- N < 10.
exam(N, R) :- exam(N // 10, S),
                       R is S + 1.