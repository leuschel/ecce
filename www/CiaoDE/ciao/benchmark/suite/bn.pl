:- module(bn, [do_bignums/0]).

:- use_module(library(prolog_sys)).
:- use_module(library(format)).

do_bignums:-
        N = 73,
        Exp = 30111,
        statistics(runtime, _),
        exponential_naive(N, Exp, _R),
        statistics(runtime, [_|T1]),
        display('Naïve exponential: '),
        display(T1),
        display(' milliseconds'),
        nl,
        statistics(runtime, _),
        exponential_div(N, Exp, _Res),
        statistics(runtime, [_|T2]),
        display('Divide-and-conquer exponential: '),
        display(T2),
        display(' milliseconds'),
        nl.
%        format("Divide-and-conquer exponential: ~d milliseconds~n", [T2]).


%% exponential(Base, Exp, Res): Be smart and split Exp in halves

exponential_div(_Base, 0, 1).
exponential_div(Base, Exp, Res):-
        Exp > 0,
        HalfExp is Exp // 2,
        exponential_div(Base, HalfExp, HalfRes),
        (
            Exp mod 2 =:= 0 ->
            Res is HalfRes*HalfRes
        ;
            Res is HalfRes*HalfRes*Base
        ).

exponential_naive(_Base, 0, 1).
exponential_naive(Base, Exp, Res):-
        Exp > 0,
        NewExp is Exp - 1,
        exponential_naive(Base, NewExp, PartRes),
        Res is PartRes * Base.
