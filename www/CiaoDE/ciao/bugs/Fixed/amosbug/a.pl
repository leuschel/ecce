:- module(a, [main/0], []).

:- use_module(b).
:- use_module(c).

main :-
	display(ok), nl.
