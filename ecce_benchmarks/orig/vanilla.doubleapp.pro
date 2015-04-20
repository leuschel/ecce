/* file: vanilla.doubleapp.pro */

solve([]).
solve([Head|Tail]) :-
	claus(Head,Body),
	solve(Body),
	solve(Tail).


claus(doubleapp(X,Y,Z,R),[app(X,Y,I),app(I,Z,R)]).
claus(tripleapp(X,Y,Z,W,R),[app(X,Y,I),app(I,Z,I2),app(I2,W,R)]).

claus(app([],L,L),[]).
claus(app([H|X],Y,[H|Z]),[app(X,Y,Z)]).

claus(concat([],[]),[]).
claus(concat([H|L],CL), [app(H,CL1,CL),concat(L,CL1)]).


