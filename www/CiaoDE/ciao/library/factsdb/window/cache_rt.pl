
:- module(cache_rt,[cache_call/3],[]).

:- use_module(library(dynamic)).
:- use_module(library(read)).

:- multifile '$cache$cached_goal'/4.

:- data cached_goal/3.

cache_call(F,A,Args):-
	'$cache$cached_goal'(F,A,File,W),
	select_cache_call(W,F,A,Args,File).

select_cache_call(0,_F,_A,H,File):- !,
	open(File,read,S),
	repeat,
	  read(S,X),
	  ( X==end_of_file,
	    !,
	    close(S),
	    fail
	  ; X=H ).
select_cache_call(W,F,A,Args,File):-
	W > 0,
	open(File,read,S),
	( retract_fact(cached_goal(F,A,N)) -> true ; N=0 ),
	N1 is N+1,
	asserta_fact(cached_goal(F,A,N1)),
	rename(F,N1,F1),
	dynamic(F1/A),
	functor(G,F1,A),
	G=..[_|Args],
	functor(T,F1,A),
	repeat,
	  retractall(T),
	  load_window(F1,F,A,S,W,E),
	  ( call(G)
	  ; ( var(E)
	    -> fail
	     ; close(S)
	    ), !,
	    fail
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
