:- module(
	fibo,
	[
	    testN/3,         % nondeterminism
	    testD/3,         % determinism
	    testS/3          % sequential
	],
	[andprolog]
	 ).

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format), [format/2]).


fibo_parN(0, 0, _).
fibo_parN(1, 1, _).
fibo_parN(N, F, Level) :-
        N > 1,
        (
            N =< Level ->
            fibo_seq(N, F)
        ;
            N1 is N - 1,
            N2 is N - 2,
            fibo_parN(N1, F1, Level) & fibo_parN(N2, F2, Level),
            F is F1 + F2
        ),
	!.

fibo_parD(0, 0, _).
fibo_parD(1, 1, _).
fibo_parD(N, F, Level) :-
        N > 1,
        (
            N =< Level ->
            fibo_seq(N, F)
        ;
            N1 is N - 1,
            N2 is N - 2,
            fibo_parD(N1, F1, Level) '&!' fibo_parD(N2, F2, Level),
            F is F1 + F2
        ),
	!.

fibo_seq(0, 0).
fibo_seq(1, 1).
fibo_seq(N, F) :-
        N > 1,
        N1 is N - 1,
        N2 is N - 2,
        fibo_seq(N1, F1),
        fibo_seq(N2, F2),
        F is F1 + F2.

testN(N, F, Time) :-
        statistics(walltime, _),
        fibo_parN(N, F, 15),  %% 15 is just an example threshold
        statistics(walltime, [_,Time]).

testD(N, F, Time) :-
        statistics(walltime, _),
        fibo_parD(N, F, 15),  %% 15 is just an example threshold
        statistics(walltime, [_,Time]).

testS(N, F, Time) :-
        statistics(walltime, _),
        fibo_seq(N, F),
        statistics(walltime, [_,Time]).


