rev([],[],Y,Y).
rev(W,[T|X],Y,Z) :- rev(W2,X,Y2,Z), rev2(T,W,W2,Y,Y2,Z).

rev2(T,[T],[],Y,[T|Y],[T|Y]).
rev2(T,[S|W],[S|W2],Y,Y2,[S|Z]) :- rev2(T,W,W2,Y,Y2,Z).

rev_last(L,a) :- rl(L,[a],R,a).
rl([],A,A,a) :- last(A,a).
rl([H|T],A,R,a) :- rl(T,[H,A],R,a).

last([a],a).
last([H|T],a) :- last(T,a).
