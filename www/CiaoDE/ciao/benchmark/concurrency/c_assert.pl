
:- dynamic odd/1, even/1.

conc_assert :-
        size(Z),
        conc_assert(Z).

conc_assert(0):-
        assert(even(0)).
conc_assert(N):-
        N > 0,
        (
            N mod 2 =:= 0 ->
            assert(even(N))
        ;
            assert(odd(N))
        ),
        N1 is N - 1,
        conc_assert(N1).

recollect:-
        (odd(N);even(N)),
        write(N), nl, 
        fail.
recollect.

clear:-
        my_retractall_fact(odd(_)),
        my_retractall_fact(even(_)).

my_retractall_fact(Fact) :-
	'$current_clauses'(Fact, Root), 
        '$current_instance'(Fact, true, Root, Ptr),
	'$erase'(Ptr),
	fail.
my_retractall_fact(_).
        
 %% check:-
 %%         size_odd(O),
 %%         check_odds(O),
 %%         size_even(E),
 %%         check_evens(E).
 %% 
 %% check_odds(1):-
 %%         check(odd(1)).
 %% check_odds(N):-
 %%         N > 1,
 %%         check(odd(N)),
 %%         N1 is N - 2,
 %%         check_odds(N1).
 %% 
 %% check_evens(0):-
 %%         check(even(0)).
 %% check_evens(N):-
 %%         N > 0,
 %%         check(even(N)),
 %%         N1 is N - 2,
 %%         check_evens(N1).
 %% 
 %% check(P):-
 %%         call(P) ->
 %%         true
 %%  ;
 %%         write(P), write(' not in database.'), nl.
 %% 
 %% odds(1):-
 %%         asserta_fact(odd(1)).
 %% odds(N):-
 %%         N > 1,
 %%         asserta_fact(odd(N)),
 %%         N1 is N - 2,
 %%         odds(N1).
 %% 
 %% evens(0):-
 %%         asserta_fact(even(0)).
 %% evens(N):-
 %%         N > 0,
 %%         asserta_fact(even(N)),
 %%         N1 is N - 2,
 %%         evens(N1).
 %% 
 %% 
 %%         
 %% size_odd(1999).
 %% size_even(E):-
 %%         size_odd(O),
 %%         E is O + 1.

size(2000).
