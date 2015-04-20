
:- use_module(dyn).
:- use_module(builtin).

main(_) :-
	display('*'), nl,
	d(X),
	display(X), nl.
