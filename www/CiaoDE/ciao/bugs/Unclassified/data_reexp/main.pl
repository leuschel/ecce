
:- use_module(m1,[m1doit/1]).

main :-
	m1doit(X),
	display(X).
