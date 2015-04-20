:- module(lqsort, [test/2], [lazy]).

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(lists), [append/3]).
:- use_module(library('lazy/lazy_lib'), _).
:- use_module(library(random)).


:- lazy qsort/2.
qsort([X|L], R) :-
	partition(L, X, L1, L2),
	qsort(L2, R2),
	qsort(L1, R1), 
        append(R1, [X|R2], R).
qsort([], []).

:- lazy partition/4.
partition([], _B, [], []).
partition([E|R], C, [E|Left1], Right):- 
	E < C,
	partition(R, C, Left1, Right).
partition([E|R], C, Left, [E|Right1]):-
	E >= C,
	partition(R, C, Left, Right1).

:- lazy gen_list/2.
gen_list(0, []).
gen_list(X, [N|T]) :-
	X > 0,
	random(1, 1000000, N),
	gen_list(X-1, T).

test(X, Res) :-
	gen_list(X, L),
	qsort(L, LOrd),
	take(X, LOrd, Res).
