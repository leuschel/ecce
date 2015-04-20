:- concurrent proceed/2.
:- concurrent p/1.

:- use_module(library(concurrency)).
:- use_module(library(system)). 
:- use_module(library(random)). 

%% MISC

not(X) :- X,!,fail.
not(X).

%% CONSUMER
 
consumer(N):-
	write(c(N)),
	nl,
	consumer2(N).

consumer2(N) :-
	repeat,
	retract_fact(proceed(N,X)),
        display(proceeding(N,X)),
	nl,
	consumer2(N).

start_consumers(0) :- !.
start_consumers(N) :- 
	eng_call(consumer(N), create, create),
	M is N - 1,
	start_consumers(M).


%% PRODUCER

producer(N,C):-
	asserta_fact(p(N)),
	pause(10),
	producer2(N,C).

producer2(N,C) :-
	repeat,
	pause(1),
        random(1,C,I), %%not(proceed(I,N)),
	asserta_fact(proceed(I,N)),
	producer2(N,C).

start_producers(0,_) :- !.
start_producers(N,C) :- 
	eng_call(producer(N,C), create, create),
	M is N - 1,
	start_producers(M,C).

%% START

start(C,P) :-
	start_consumers(C),
	start_producers(P,C).













