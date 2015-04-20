% Execute test and Ciao breaks...

:- module(_, _, []).

:- concurrent a/0.

main :-
	asserta_fact(a),
	retract_fact_nb(a),
	display(ok), nl,
	main.
