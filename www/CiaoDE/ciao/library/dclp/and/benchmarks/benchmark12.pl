:- use_package(library('dclp/and')).
:- use_module(library(prolog_sys), [statistics/2]).

main(X,Y,Z) :-
	store([X, Y], 1),
	store([Z], 2),
	(X in 0..4,
	 Y in 0..2,
	 X .>. Y,
	 X .=. 2 * Y) @ 1,
	(Z in 2..6,
	 X in 0..4,
	 Y in 0..2,
	 Z .=. X + Y,
	 Z .>. 3) @ 2,
	d_labeling([X,Y,Z]).
