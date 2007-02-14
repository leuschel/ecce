pairl([]).
pairl([A|X]) :- oddl(X).
oddl([A|X]) :- pairl(X).

delete(X,[X|T],T).
delete(X,[Y|T],[Y|DT]) :-
 X\==Y, delete(X,T,DT).

error(X,L) :- pairl(L),delete(X,L,DL),pairl(DL).



delete2(X,[X|T],T).
delete2(X,[Y|T],DT) :-
 \+(X=Y), delete2(X,T,DT).


error2(X,L) :- pairl(L),delete2(X,L,DL),pairl(DL).