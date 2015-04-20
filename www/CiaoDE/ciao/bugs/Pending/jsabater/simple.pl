:- module(simple, [main/0], []).

:- use_module(library(concurrency)).


:- concurrent proceed/1.
         
main:-
        display('SSSTTTAAARRRTTTIIINNNGGG!!!'), nl,
        (I = 1; I = 2; I = 3),
         \+ current_fact_nb(proceed(pair(I))),
         display(proceed(pair(I))),
         nl,
         assertz_fact(proceed(pair(I))),
         display(asserted(pair(I))),
         nl,
         fail.
main:-
        display(done), nl.
