:- module(_, _, []).
:- use_module(engine(internals)).

% This is a bug in the memory consuption of catch mechanism

main :-
	'$metachoice'(C),
	catch(a, E, handle(E)),
	'$metachoice'(C2),
	( C = C2 ->
	    display('Ok, catch does not leave unnecessary choice points'), nl
	; display('Wrong, catch does leave unnecessary choice points'), nl
	).

a.

handle(E).
