:- module(_, [test/0], []).

:- use_module(library(system)).
:- use_module(library(concurrency)).
:- use_module(library(format)).

:- use_module(make_assertion).
:- use_module(make_retract).



test:-
        eng_call(wait_for_fact, create, create, Id),
        assert_it(Id),
        eng_wait(Id),
        eng_release(Id).
