:- use_package(library('dclp/and')).
:- use_module(library(prolog_sys), [statistics/2]).

main(X,Y,Z) :-
 	store([X], 1),
 	store([Y], 2),
 	store([Z], 3),
	(X in 0..4,
	 Y in 0..2,
	 X .=. 2 * Y) @ 1,
	(X in 0..4,
	 Y in 2..2,
	 X .>. Y) @ 2,
	(Z in 2..6,
	 X in 0..4,
	 Y in 0..2,
	 Z .=. X + Y,
	 Z .>. 3) @ 3,
	d_labeling([X,Y,Z]).
