grandmother(X,Y) :- mother(X,Z), parent(Z,Y).
grandfather(X,Y) :- father(X,Z), parent(Z,Y).
parent(X,Y) :- mother(X,Y).
parent(X,Y) :- father(X,Y).
mother(sue,simon).
mother(sue,patrick).
mother(monica,sue).

test(X) :- grandmother(X,simon).