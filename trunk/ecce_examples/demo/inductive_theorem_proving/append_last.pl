append([],L,L).
append([H|X],Y,[H|Z]) :- append(X,Y,Z).

last([X],X).
last([H,H2|T],X) :- last([H2|T],X).

al(X,A,Last) :- append(X,[A],Xs),last(Xs,Last).

al_fail(X) :- al(X,a,b).
