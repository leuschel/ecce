
:- module(name,[ name/3 ],[ ]).

name(N,X,Y):-
	display(N),
	name(X,Y),
	display('...ok'), nl.
