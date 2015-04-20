:- module(_, [assert_it/1,term_passed_around/2], []).

:- use_module(library(system)).
:- use_module(library(format)).

:- concurrent term_passed_around/2.

before_assert(1).

assert_it(Id):-
        before_assert(A),
        pause(A),
        format("Asserting Id = ~w~n",[Id]),
        assertz_fact(term_passed_around(Id, 76)),
        format("Asserted~n",[]).
