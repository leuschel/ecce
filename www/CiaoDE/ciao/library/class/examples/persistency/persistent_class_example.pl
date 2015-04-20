
:- class(persistent_class_example).

:- inherit_class(library('class/library/persistent')).

:- persistent(attr/1).
:- data counter/1.
counter(0).

persistent_class_example(Streamer) :-
	persistent(Streamer).

:- export(add/1).
:- export(remove/1).
:- export(get/1).
:- export(count/1).

add(X) :-
	assertz_fact(attr(X)),
	retract_fact(counter(Old)),
	NewCount is Old+1,
	asserta_fact(counter(NewCount)).

remove(X) :-
	retract_fact(attr(X)),
	retract_fact(counter(Old)),
	NewCount is Old-1,
	asserta_fact(counter(NewCount)).

get(X) :-
	attr(X).

count(X) :-
	counter(X).

persistent_state_loaded :-
	attr(_),
	retract_fact(counter(Old)),
	NewCount is Old+1,
	asserta_fact(counter(NewCount)),
	fail.
