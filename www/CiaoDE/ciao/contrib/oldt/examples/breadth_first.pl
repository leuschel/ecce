
:- dynamic path/2, edge/2.

path(X,Y):- path(Y,X).
path(X,Y):- edge(X,Y).
path(X,Y):- path(X,Z), path(Z,Y).

edge(a,b).
edge(b,c).
edge(c,a).

%% :- oldt(user:path(X,Y)), displayq(path(X,Y)), nl, fail.
