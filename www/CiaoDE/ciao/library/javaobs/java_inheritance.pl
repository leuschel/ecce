
%%
%% instance_of predicate for java_mod.pl and 
%% module syntax java-prolog interfaces.
%%

:- module(java_inheritance,[]).
:- export(java_instance_of/2).
:- export(set_java_instance_of/2).
:- export(set_java_parents/2).

:- data      java_instance/2.
:- data      java_parent/2.

java_instance_of(Instance, Class) :-
	java_instance(Instance, Class).

java_instance_of(Instance, Class) :-
	java_instance(Instance, X),
	( Class = X
	; java_parents(X, Class)
	).

set_java_instance_of(Instance, Class) :-
	assertz_fact(java_instance(Instance, Class)).

set_java_parents(Class, [Parent|Parents]) :-
	assertz_fact(java_parent(Class, Parent)),
	set_java_parents(Class, Parents).

set_java_parents(_, []).

java_parents(Class, Superclass):-
    java_parent(Class, Superclass).

java_parents(Class, Superclass):-
    java_parent(Class, X),
    ( Superclass = X
    ; java_parents(X, Superclass)
    ).
