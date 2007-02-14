
rev([],[]).
rev([X|Xs],Y) :-
	rev(Xs,Z),
	append(Z,[X],Y).


append([],L,L).
append([H|X],Y,[H|Z]) :- append(X,Y,Z).