map(P,[],[]).
map(P,[H|T],[PH|PT]) :-
     C=..[P,H,PH],
     call(C),map(P,T,PT).
     
inv(0,1).
inv(1,0).


p([],[]).
p([H|T],[a|PT]) :- q(T,TP).

q([],[]).
q([b|T],QT) :- p(T,QT).