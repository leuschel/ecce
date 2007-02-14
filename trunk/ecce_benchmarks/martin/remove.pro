remove:rr(X,Y) :- remove:r(X,T), remove:r(T,Y).

remove:r([],[]).
remove:r([X],[X]).
remove:r([X,X|T],[X|T1]) :- remove:r(T,T1).
remove:r([X,Y|T],[X|T1]) :- X \== Y, remove:r([Y|T],T1).

