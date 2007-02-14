
rr(X,Y) :- r(X,T), r(T,Y).

r([],[]).
r([X],[X]).
r([X,X|T],[X|T1]) :- r(T,T1).
r([X,Y|T],[X|T1]) :- X \== Y, r([Y|T],T1).
