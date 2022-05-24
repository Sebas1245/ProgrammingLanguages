masculine(john).
feminie(mary).
father(john, mary).
daughter(mary, john).
mother(mary, peter).
gift(mary, toy, peter).
sum(0,1,1).

employee(john, 22, e1).
employee(peter, 19, e2).
employee(rose, 22, e3). 
student(rose, informatics).
student(albert, pharmacy).
student_workder(X) :-
    student(X, Y),
    employee(X, Z, W).