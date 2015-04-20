:- module(_, [uc1/3,uc2/3], []).

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_package(clpq).

 %% it turns out that unifying (independent) constrained variables is
 %% awfully expensive!  Let us measure it.

uc1(N, TimeConst, TimeUnif):-
        statistics(runtime, _),
        variables1(N, List1),
        variables1(N, List2),
        statistics(runtime, [_, TimeConst]),
        List1 = List2,
        statistics(runtime, [_, TimeUnif]).

uc2(N, TimeConst, TimeUnif):-
        statistics(runtime, _),
        variables2(N, List1),
        variables2(N, List2),
        statistics(runtime, [_, TimeConst]),
        List1 = List2,
        statistics(runtime, [_, TimeUnif]).


variables1(N, List):-
        length(List, N),
        set_constraints1(List).


variables2(N, List):-
        length(List, N),
        set_constraints2(List).




set_constraints2([_X, _Y]):- !.
set_constraints2([X, Y, Z|Rest]):-
        X .=. Y + Z,
        X .>. Y,
        X .>. Z,
        set_constraints2([Y,Z|Rest]).


set_constraints1([]).
set_constraints1([X|Xs]):-
        X .>. 0,
        set_constraints1(Xs).
