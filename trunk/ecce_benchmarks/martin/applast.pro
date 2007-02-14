/* file applast.pro */

applast:applast(L,X,Last) :- applast:append(L,[X],LX),applast:last(Last,LX).


applast:last(X,[X]).
applast:last(X,[H|T]) :- applast:last(X,T).

applast:append([],L,L).
applast:append([H|L1],L2,[H|L3]) :- applast:append(L1,L2,L3).
