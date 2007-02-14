doubleapp(X,Y,Z,XYZ) :- append(X,Y,XY), append(XY,Z,XYZ).

append([],L,L).
append([H|X],Y,[H|Z]) :- append(X,Y,Z).

rev([],X,X).
rev([H|X],A,R) :- rev(X,[H|A],R).

p :- q.
q.
