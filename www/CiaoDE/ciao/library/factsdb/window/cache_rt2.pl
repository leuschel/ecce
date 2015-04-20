
:- module(cache_rt2,[cache_call/3],[]).

:- use_module(library(dynamic)).
:- use_module(library(read)).

:- multifile '$cache$cached_goal'/4.

:- data cached_goal/3.

cache_call(F,A,Args):-
	'$cache$cached_goal'(F,A,File,W),
	open(File,read,S),
	( retract_fact(cached_goal(F,A,N)) -> true ; N=0 ),
	N1 is N+1,
	asserta_fact(cached_goal(F,A,N1)),
	rename(F,N1,F1),
	dynamic(F1/A),
	functor(T,F1,A),
	T=..[_|Args],
	repeat(F1,F,A,T,S,W).

repeat(F,P,N,T,S,W):-
	load_window(F,P,N,S,W,E), !,
	( call(T)
	; ( var(E)
	  -> functor(G,F,N),
	     retractall(G),
	     repeat(F,P,N,T,S,W)
	   ; close(S), 
	     fail
	  )
	).

load_window(_,_,_,_,0,_):- !.
load_window(F,P,N,S,W,E):-
	W > 0,
	functor(T,F,N),
	T=..[_|Args],
	Q=..[P|Args],
	repeat,
	  read(S,R),
	  nonvar(R),
	  ( R=end_of_file
	  -> E=R
	   ; R=Q,
	     assertz(T),
	     W1 is W-1,
	     load_window(F,P,N,S,W1,E)
	  ), !.

rename(F,N1,F1):-
	number_codes(N1,C),
	atom_codes(A,C),
	atom_concat(F,A,F1).
