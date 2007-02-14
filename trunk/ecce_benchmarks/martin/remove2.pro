remove2:rr(X,Y) :- remove2:f(X,T), remove2:f(T,Y).

remove2:f([],[]).
remove2:f([A|T],Y) :- remove2:h(A,T,Y).

remove2:h(A,[],[A]).
remove2:h(A,[B|S],Y) :- remove2:g(A,B,[B|S],S,Y).

remove2:g(A,A,_,S,[A|Y]) :- remove2:f(S,Y).
remove2:g(A,B,T,_,[A|Y]) :- A \== B,remove2:f(T,Y).

