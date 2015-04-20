:- use_package(library('dclp/and')).
:- use_module(library(prolog_sys), [statistics/2]).

main(X) :-
	store([X], 1),
	(X in 0..1,
	 X .>. 0) @ 1,
	d_labeling([X]).
