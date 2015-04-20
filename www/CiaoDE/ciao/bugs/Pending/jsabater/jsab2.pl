:- concurrent proceed/2.

:- use_module(library(system)). 
:- use_module(library(random)).
:- use_module(library(concurrency)).

producer(N,0) :- display(end(N)),nl. 
producer(N,M) :-
        wait_for,
	M2 is M - 1,
	producer(N,M2). 

wait_for:-
        N = 0,
	repeat,
        random(1,100,I),
       	display(looking_for(N,I)),
        nl,
	not(current_fact_nb(proceed(N,I))),
	display(proceed(N,I)),
	nl,
	asserta_fact(proceed(N,I)),
	display(asserted(proceed(N,I))),
	nl.

wf:-
        N = 0,
	repeat,
        random(1,100,I),
       	display(looking_for(N,I)),
        nl,
        (
            current_fact_nb(proceed(N,I)) ->
            fail
        ;
            display(proceed(N,I)),
            nl,
            asserta_fact(proceed(N,I)),
            display(asserted(proceed(N,I))),
            nl
        ).



a:- asserta_fact(proceed(p(0, 0))).

start(0). 
start(N) :-
	eng_call(producer(N,5),create,create),
	M is N - 1,
	start(M).

not(Goal) :-
        call(Goal), 
        !,
        fail.
not(_Goal).
