:- module(_, [test/0], []).

:- concurrent term_passed_around/2.

:- use_module(library(system)).
:- use_module(library(concurrency)).
:- use_module(library(format)).

 %% before_retract(3).
 %% before_assert(1).

before_retract(1).
before_assert(3).

test:-
        eng_call(wait_for_fact, create, create, Id),
        before_assert(A),
        pause(A),
        format("Asserting Id = ~w~n",[Id]),
        assertz_fact(term_passed_around(Id, 76)),
        format("Asserted~n",[]),
        eng_wait(Id),
        eng_release(Id).

wait_for_fact:-
        eng_self(Id),
        before_retract(R),
        pause(R),
        format("Retracting Id = ~w~n",[Id]),
        retract_fact(term_passed_around(Id, _Num)),
        format("Retracted Id = ~w~n",[Id]).
