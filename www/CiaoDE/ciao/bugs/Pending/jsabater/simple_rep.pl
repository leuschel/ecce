:- concurrent a/1.

wait_for(A):-
        \+ current_fact_nb(a(A)).

add(A):-
        asserta_fact(a(A)).
