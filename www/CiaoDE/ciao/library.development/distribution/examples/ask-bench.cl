p(X,Y) :-
    X = f(_),
    ask(X=Y).
q(X,Y) :-
    ask(X = f(A)),
    A = 1,
    X = Y.

pq(X,Y) :- p(X,Y) &, q(X,Y).
qp(X,Y) :- q(X,Y) &, p(X,Y).


r(F,A,C) :- F = f(A,b,C).
s(F,A,C) :- F = f(A,1,C).

p(F1,F2,R) :- ask(F1=F2), !, R = F1.
p(F1,F2,R) :- ask(F1<>F2), !, R = p(F1,F2).

rsp(R) :-r(F1,A1,C1) &, s(F2,A2,C2) &, p(F1,F2,R), A1=A2, C1=C2.
ssp(R) :-s(F1,A,C) &, s(F2,A,C) &, p(F1,F2,R).
