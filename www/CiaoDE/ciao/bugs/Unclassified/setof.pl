
p(X,L):- setof( X, r(X,Y), L).

q(X,L):- setof( X, Y^r(X,Y), L).

r(a,1).
r(a,2).
r(a,3).
r(b,1).
r(b,2).
r(b,3).
