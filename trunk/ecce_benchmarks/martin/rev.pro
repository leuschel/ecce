
rev:rev([],[]).
rev:rev([X|Xs],Y) :-
	rev:rev(Xs,Z),
	rev:append(Z,[X],Y).


rev:append([],L,L).
rev:append([H|X],Y,[H|Z]) :- rev:append(X,Y,Z).