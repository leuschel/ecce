:- concurrent proceed/1.

:- use_module(library(random)).

wait_for:-
	repeat,
        random(1,100,I),
       	display(looking_for(I)),
        nl,
	not(current_fact_nb(proceed(I))),
	display(proceed(I)),
	nl,
	asserta_fact(proceed(I)),
	display(asserted(proceed(I))),
	nl.


a:- asserta_fact(proceed(0, 0)).

not(Goal) :-
        call(Goal), 
        !,
        fail.
not(_Goal).
