
:- use_module(library('compiler/c_itf')).
:- use_module(engine(internals)).

main :-
	builtin_module(M),
	process_file(engine(M), b, any, list, false, false, true ),
	fail.

true(_).

list(Base):-
	exports(Base,F,A,_DefType,_Meta),
	display(F),
	display('/'),
	display(A), nl,
	fail.
list(_).
