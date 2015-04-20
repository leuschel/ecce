:- use_package(library('dclp/and')).
:- use_module(library(prolog_sys), [statistics/2]).

main(X,Y) :-
	store([X], 1),
	store([Y], 2),
	([X,Y] in 0..1,
	 X .>. Y) @ 1,
	([X,Y] in 0..1) @ 2,
	d_labeling([X,Y]).
