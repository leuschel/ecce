:- module(_, _, []).
:- use_module(engine(internals)).

main :-
	p(10000).

p(0) :- !.
p(N) :-
	catch(q, _E, x),
	'$metachoice'(C),
	display(C), nl,
	N1 is N - 1,
	p(N1).

x.

q.
