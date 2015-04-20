
:- use_module(library(write)).
:- use_module(library(dummy)).

:- data p/1.

items(10000).

main:-
        items(N),
        statistics(runtime, _),
        do_loop_1(N),
        do_loop_2(N),
        statistics(runtime, [_,T]),
        write(asserting_and_retracting(T)), nl,
        do_loop_3(N),
        statistics(runtime, [_, T1]),
        write(process_loop(T1)), 
        nl.

do_loop_1(0).
do_loop_1(N):-
        N > 0,
        asserta_fact(p(N)),
        N1 is N - 1,
        do_loop_1(N1).

do_loop_2(0).
do_loop_2(N):-
        N > 0,
        retract_fact(p(N)),
        N1 is N - 1,
        do_loop_2(N1).

do_loop_3(N):-
        N < 1, !.
do_loop_3(N):-
        N > 0,
        _ is N*N,
        X1 is N - 1,
        do_loop_3(X1).
