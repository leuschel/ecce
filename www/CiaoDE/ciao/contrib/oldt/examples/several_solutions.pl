
:- dynamic s/2.

s(a,b).
s(X,Y):- s(Y,X).

%% :- oldt(user:s(X,Y)), displayq(s(X,Y)), nl, fail.
