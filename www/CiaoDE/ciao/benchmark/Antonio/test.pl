
:- use_package(pure).
:- use_module(engine(arithmetic),[is/2]).
:- use_module(engine(io_basic),[display/1,nl/0]).
:- use_module(library(arrays)).
:- use_module(library(lists),[append/3]).
:- use_module(library(prolog_sys)).

l(N) :- time_list0(N,L,T1), !, 
	display(creating), display(T1), nl,
	N1 is round(N/2),
	time_elem(N1,L,_,T),
	display(accessing), display(T), nl.

a(N):-  new_array(A),
	time_array0(N,A,B,T1), !,
	display(creating), display(T1), nl,
	N1 is round(N/2), 
	time_aref(N1,B,_,T),
	display(accessing), display(T), nl.

time_list(N,L,T):-
	statistics(runtime,_),
	list(N,L),
	statistics(runtime,[_,T]).

list(0,[]).
list(N,[_|Xs]):- N1 is N-1, list(N1,Xs).

time_list0(N,L,T):-
	statistics(runtime,_),
	list0(N,[],L),
	statistics(runtime,[_,T]).

list0(0,A,A).
list0(N,A,C):- append(A,[_],B), N1 is N-1, list0(N1,B,C).

time_elem(N,L,E,T):-
	statistics(runtime,_),
	elem(L,N,E),
	statistics(runtime,[_,T]).

elem([X|_],1,X):- !.
elem([_|Xs],N,X):- N1 is N-1, elem(Xs,N1,X).

time_array(N,A,B,T):-
	statistics(runtime,_),
	array(N,A,B),
	statistics(runtime,[_,T]).

array(0,A,A).
array(N,A,C):- aset(N,A,_Element,B), N1 is N-1, array(N1,B,C).

time_array0(N,A,B,T):-
	statistics(runtime,_),
	aset(N,A,_Element,B),
	statistics(runtime,[_,T]).

time_aref(N,A,E,T):-
	statistics(runtime,_),
	aref(N,A,E),
	statistics(runtime,[_,T]).
