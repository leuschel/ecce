:- module(_, [main/1], []).

:- use_module(library(prolog_sys)).
:- use_module(library(system)).

:- data q/2.
:- data p/2.

items(100000).

%% items(10000).


write_q(0).
write_q(N):-
        N > 0,
        asserta_fact(q(N, a)),
        assertz_fact(q(N, a)),
        N1 is N - 1,
        write_q(N1).

read_q(0).
read_q(N):-
        N > 0,
        retract_fact(q(N, a)),
        retract_fact(q(N, a)),
        N1 is N - 1,
        write_q(N1).

write_p(0).
write_n(N):-
        N > 0,
        asserta_fact(p(N, a)),
        assertz_fact(p(N, a)),
        N1 is N - 1,
        write_p(N1).

read_p(0).
read_p(N):-
        N > 0,
        retract_fact(p(N, a)),
        retract_fact(p(N, a)),
        N1 is N - 1,
        write_p(N1).



%% :- data a/1, b/1.

main([_]):-
        items(N),
        statistics(runtime, _),
        eng_call(write_q(N), create, create, Id1),
        eng_call(write_p(N), create, create, Id2),
        eng_call(write_q(N), create, create, Id3),
        eng_call(write_p(N), create, create, Id4),
        eng_wait(Id1),
        eng_wait(Id2),
        eng_wait(Id3),
        eng_wait(Id4),
        eng_release(Id1),
        eng_release(Id2),
        eng_release(Id3),
        eng_release(Id4),
        eng_call(read_q(N), create, create, IdR1),
        eng_call(read_p(N), create, create, IdR2),
        eng_call(read_q(N), create, create, IdR3),
        eng_call(read_p(N), create, create, IdR4),
        eng_wait(IdR1),
        eng_wait(IdR2),
        eng_wait(IdR3),
        eng_wait(IdR4),
        eng_release(IdR1),
        eng_release(IdR2),
        eng_release(IdR3),
        eng_release(IdR4),
        statistics(runtime, [_,T]),
        display('Used '),
        display(T),
        display(' ms.'),
        nl.

main([]):-
        items(N),
        statistics(runtime, _),
        write_q(N),
        read_q(N),
        statistics(runtime, [_,T]),
        display('Used '),
        display(T),
        display(' ms.'),
        nl.
