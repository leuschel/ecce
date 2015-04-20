
:- module(call_at_module,[]).

:- export(dynamic_access/0).
:- export(static_access/0).

:- data attribute/1.

attribute(1).

dynamic_access :-
	attribute(_).

static_access :-
	non_attribute(_).

non_attribute(1).
