
:- use_package('cache/dynamic/cache').
:- cache(p/1,file_p).
:- cache(q/1,file_q).

g(X):-
	sol(X).
g(X):-
	assertz_fact(p(z)),
	sol(X).


sol(X):-
	p(X),
	display(p(X)), nl,
	q(X),
	display(q(X)), nl.
