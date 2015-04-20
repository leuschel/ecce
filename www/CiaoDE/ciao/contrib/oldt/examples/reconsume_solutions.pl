
:- dynamic r/2.

r(X,Y):- r(Y,X).
r(a,b).

%% :- oldt(user:r(X,Y)), displayq(r(X,Y)), nl, fail.
