
solve([]).
solve([H|T]) :- claus(H,B), solve(B), solve(T).

claus(f(0,_B,1),[]).
claus(f(s(X),B,Res),[f(X,B,R2),Res is B*R2]).
claus(g(X,R),[f(s(s(0)),X,R1), f(X,2,R2), R is R1 + R2]).
claus(is(X,Exp),[]) :- X is Exp.

test(X,R) :- solve([g(X,R)])
