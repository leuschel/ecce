
:- class(assrt_at_class).

:- export(callme/1).

:- data attribute/1.

callme(N) :-
	do_assert(N),
	do_retract.

do_assert(0) :- !. 
do_assert(N) :-
	M is N-1,
	asserta_fact(attribute(foo)),
	do_assert(M).

do_retract :-
	retract_fact(attribute(_)),
	fail.
do_retract.
