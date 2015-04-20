:- module(_, [wait_for_fact/0], []).

:- use_module(library(concurrency)).
:- use_module(library(format)).
:- use_module(library(system)).

:- use_module(make_assertion).

before_retract(3).

wait_for_fact:-
        eng_self(Id),
        before_retract(R),
        pause(R),
        format("Retracting Id = ~w~n",[Id]),
        retract_fact(term_passed_around(Id, _Num)),
        format("Retracted Id = ~w~n",[Id]).
