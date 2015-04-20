rr(X,Y) :- f(X,T), f(T,Y).

f([],[]).
f([A|T],Y) :- h(A,T,Y).

h(A,[],[A]).
h(A,[B|S],Y) :- g(A,B,[B|S],S,Y).

g(A,A,_,S,[A|Y]) :- f(S,Y).
g(A,B,T,_,[A|Y]) :- A \== B,f(T,Y).

