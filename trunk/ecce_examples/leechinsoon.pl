p :- f(0,s(0),s(s(0)),s(s(s(s(0))))).

f(X,X,X,X).
f(X1,X2,X3,X4) :- f(X2,X3,X4,X4).
f(X1,X2,X3,X4) :- f(X1,X2,X3,X4).