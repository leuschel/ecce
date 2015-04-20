:- module(usefoo,[main/0]).

:- use_module('foo/foo.pl').

main :-
	X=5,
	sqr(X,Y),
	display(Y),nl.
