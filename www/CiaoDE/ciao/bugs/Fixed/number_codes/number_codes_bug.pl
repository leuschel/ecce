% Enter 2'11111. Instead of 31, the program reads 11111.

:- module(_, _, []).

:- use_module(library(strings)).

main :-
	number_codes(N, 2, "11111"),
	display(N), nl.
