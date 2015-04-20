:- concurrent proceed/1.

:- use_module(library(system)). 
:- use_module(library(random)).
:- use_module(library(concurrency)).

producer(N,0) :- display(end(N)),nl. 
producer(N,M) :-
	repeat,
        random(1,100,I),
	\+ current_fact_nb(proceed(pair(N,I))),
	display(proceed(pair(N,I))),
	nl,
	asserta_fact(proceed(pair(N,I))),
	M2 is M - 1,
	producer(N,M2). 

start(0). 
start(N) :-
	eng_call(producer(N,5),create,create),
	M is N - 1,
	start(M).
