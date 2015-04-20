:- use_package(library('dclp/and')).
:- use_module(library(prolog_sys), [statistics/2]).

main(X,Y) :-
	store([X], 1),
	store([Y], 2),
	(X in 1..2,
	 X .>. Y) @ 1,
	(Y in 1..2,
	 X .<>. Y) @ 2,
	d_labeling([X,Y]).
