path(X,Y) :- edge(X,Y).
path(X,Y) :- edge(X,Z),path(Z,Y).

edge(a,b).
edge(a,c).
edge(c,b).
edge(c,a).


initial(a).

/* from P&P JLP99 paper : */
reach(X) :- initial(X).
reach(X) :- reach(Y), edge(Y,X).