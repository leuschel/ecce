:- use_package(library('dclp/and')).
:- use_module(library(prolog_sys), [statistics/2]).

main(X,Y,Z) :-
	store([X], 1),
	store([Y], 2),
	store([Z], 3),
	(X in 0..16,
	 Y in 0..12,
	 X .>. Y) @ 1,
	(X in 0..16,
	 Y in 0..12,
	 X .=. 2 * Y) @ 2,
	(Z in 1..12 .&. 15..17,
	 X in 8..16,
	 Y in 0..12,
	 Z .=. X + Y,
	 Z .>. 12) @ 3,
	d_labeling([X,Y,Z]).
