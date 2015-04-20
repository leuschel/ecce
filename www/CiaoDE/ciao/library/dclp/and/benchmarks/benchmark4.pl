:- use_package(library('dclp/and')).
:- use_module(library(prolog_sys), [statistics/2]).

main(X,Y,Z) :-
	store([X], 1),
	store([Y], 2),
	store([Z], 3),
	(X in 0..5) @ 1,
	(Y in 0..5) @ 2,
	(X in 5..5,
	 Y in 4..5,
	 Z in 0..10,
	 Z .>. X + Y) @ 3,
	d_labeling([X,Y,Z]).
