knows_logic(X) :-
    good_student(X),teacher(Y,X),logician(Y).
good_student(jane).
good_student(tom).
logician(aristo).
logician(plato).
logician(kant).
logician(xyz).
logician(peter).
teacher(peter,tom).