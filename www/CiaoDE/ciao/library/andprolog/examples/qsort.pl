:- module(
	qsort,
	[
	    qsortN/3,    % with nondeterminism
	    qsortD/3,    % with determinism
	    qsortS/3     % sequential
	],
	[andprolog]
	 ).

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(lists), [append/3]).


qsortN(X, Y, Time) :-
        statistics(walltime, _),
	qsortN_(X, Y),
        statistics(walltime, [_,Time]).

qsortN_([X|L], R) :-
	partition(L, X, L1, L2),
	qsortN_(L2, R2) & qsortN_(L1, R1), 
        append(R1, [X|R2], R),
	!.
qsortN_([], []).

qsortD(X, Y, Time) :-
        statistics(walltime, _),
	qsortD_(X, Y),
        statistics(walltime, [_,Time]).

qsortD_([X|L], R) :-
	partition(L, X, L1, L2),
	qsortD_(L2, R2) '&!' qsortD_(L1, R1), 
        append(R1, [X|R2], R),
	!.
qsortD_([], []).

qsortS(X, Y, Time) :-
        statistics(walltime, _),
	qsortS_(X, Y),
        statistics(walltime, [_,Time]).

qsortS_([X|L], R) :-
	partition(L, X, L1, L2),
	qsortS_(L2, R2),
	qsortS_(L1, R1), 
        append(R1, [X|R2], R),
	!.
qsortS_([], []).

partition([], _B, [], []).
partition([E|R], C, [E|Left1], Right) :- 
	E < C,
	partition(R, C, Left1, Right).
partition([E|R], C, Left, [E|Right1]) :-
	E >= C,
	partition(R, C, Left, Right1).


