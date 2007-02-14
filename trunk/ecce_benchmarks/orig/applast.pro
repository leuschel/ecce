/* file applast.pro */

applast(L,X,Last) :- append(L,[X],LX),last(Last,LX).


last(X,[X]).
last(X,[H|T]) :- last(X,T).

append([],L,L).
append([H|L1],L2,[H|L3]) :- append(L1,L2,L3).

