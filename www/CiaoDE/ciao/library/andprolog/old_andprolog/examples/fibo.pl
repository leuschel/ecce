:- module(fibo, [fibo/2, test/3], [andprolog]).

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format), [format/2]).


fibo(N, F):-
        fibo_par(N, F, 10).  %% 10 is just an example threshold

fibo_par(0, 0, _).
fibo_par(1, 1, _).
fibo_par(N, F, Level):-
        N > 1,
        (
            N < Level ->
            fibo_seq(N, F)
        ;
            N1 is N - 1,
            N2 is N - 2,
            fibo_par(N1, F1, Level) & fibo_par(N2, F2, Level),
            F is F1 + F2
        ).

fibo_seq(0, 0).
fibo_seq(1, 1).
fibo_seq(N, F):-
        N > 1,
        N1 is N - 1,
        N2 is N - 2,
        fibo_seq(N1, F1),
        fibo_seq(N2, F2),
        F is F1 + F2.

test(N, F, Time):-
        statistics(walltime, _),
        fibo(N, F),
        statistics(walltime, [_,Time]).
