:- include(functions).

:- use_module(library(write)).
:- use_module(library(system)).

:- use_module(library(freeze)).

:- data wrap/2.

parallel(V, Goal) :-
	launch_goal((Goal, asserta_fact(wrap(GoalId, V))), GoalId),
	freeze(V, parallel_join(GoalId, V)).

parallel_join(GoalId, V) :-
	write(joining), nl,
	join_goal(GoalId),
	retract_fact(wrap(GoalId, V)).


:- function(naturals_from/1).
:- function(naturals/0).
naturals := naturals_from(0).
naturals_from(X) := [X|lazy naturals_from(X + 1)] :-
	write(lazy_put(X)), nl.

:- function(naturals2_from/2).
:- function(naturals2/0).
naturals2 := naturals2_from(0, 10).
naturals2_from(X, Y) := [] :-
	X > Y, !.
naturals2_from(X, Y) := [X|parallel naturals2_from(X + 1, Y)] :-
	pause(1),
	write(parallel_put(X)), nl.

:- function(prefix/2).
prefix(0, _) := [].
prefix(N, [X|Xs]) := [X|prefix(N - 1, Xs)] :-
	write(get(X)), nl.

test_lazy :-
	naturals(X),
	prefix(10, X, Y),
	write(Y), nl.

test_parallel :-
	naturals2(Z),
	prefix(4, Z, U),
	write(U), nl,
	prefix(8, Z, V),
	write(V), nl.




:- function(make/2).

make(X, Y) := (X * 2, f(Y)).

:- function(g/2).

g(X, Y) := lazy make(X,Y). 



:- function(i/1).

i(X) := X * 2.

:- function(l/1).

l(X) := lazy i(X).
