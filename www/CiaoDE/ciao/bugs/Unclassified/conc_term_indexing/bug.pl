:- module(_, [main/0, bug/0], []).

:- use_module(library('javall.new/javart')).

main:- bug.

bug:-
        java_start,
        java_create_object('java.lang.String'(hola), Object),
        display(Object),
        nl.
