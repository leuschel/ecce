:- module(lqsort_function, [test/2], [functions, lazy]).

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(lists), [nth/3, append/3]).
:- use_module(library('lazy/lazy_lib'), _).
:- use_module(library(random)). 

:- lazy function qsort/1.
qsort(X) := ~qsort_(X, []).

:- lazy function qsort_/2.
qsort_([], Acc)    := Acc.
qsort_([X|T], Acc) := ~qsort_(S, [X|~qsort_(G, Acc)])
                   :- (S, G) = ~partition(T, X).

:- lazy function partition/3.
partition([], _)    := ([], []).
partition([X|T], Y) := (S, [X|G]) :- Y < X, !, (S,G) = ~partition(T, Y).
partition([X|T], Y) := ([X|S], G) :- !, (S,G) = ~partition(T, Y).

:- lazy function gen_list/2.
gen_list(0) := [].
gen_list(X) := [~random(1,1000000)|~gen_list(X-1)] :- X > 0.

:- function test/1.
test(X) :=
	~take(X,~qsort(~gen_list(X))).
