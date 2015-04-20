 %% test_conc.pl -- 
 %% AFSID           : $__Header$
 %% Author          : Manuel Carro
 %% Created On      : Mon Jan 12 19:25:28 1998
 %% Last Modified By: MCL
 %% Last Modified On: Tue Apr  7 22:03:51 1998
 %% Update Count    : 16
 %% Status          : Unknown, Use with caution!

% :- dynamic c/1.

:- use_module(library(system)).

:- concurrent c/1.

:- dynamic p/1.



c:- 
        assert_data(c(0)).

p:- 
        assert(p(0)).


o:- 
        espera(1000000).

espera(0):- lock_atom(espera), write(acabado), nl, unlock_atom(espera).
espera(N):-
        N > 0,
        N1 is N - 1,
        espera(N1).


try_lock_atom:-
    lock_atom(wait),
    eng_call(wait_on, create, create),
    pause(1),
    unlock_atom(wait).


wait_on:-
    display('I am waiting'), nl,
    lock_atom(wait),
    display('I am continuing now!'), nl,
    unlock_atom(wait).


:- concurrent fact_to_wait_on/0.

try_lock_db:-
    eng_call(wait_on_fact, create, create),
    pause(1),
    asserta_fact(fact_to_wait_on).


wait_on_fact:-
    display('I am waiting'), nl,
    current_fact(fact_to_wait_on),
    display('I am continuing now!'), nl.
