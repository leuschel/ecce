:- use_package(library('dclp/and')).
:- use_module(library(prolog_sys), [statistics/2]).

main(X,Y,Z) :-
  	store([X], 1),
  	store([Y], 2),
  	store([Z], 3),
	(X in 1..2,
	 X .<>. Z) @ 1, 
	(Y in 2..2,
	 Y .<>. Z) @ 2,
	(Z in 1..2,
	 Z .<>. X, 
	 Z .<>. Y) @ 3,
	d_labeling([X,Y,Z]).
