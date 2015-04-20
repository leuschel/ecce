
:- module(cache_rt,[cache_call/2],[]).

:- use_module(library(read)).

cache_call(File,H):-
        open(File,read,S),
	repeat,
	  read(S,X),
	  ( X==end_of_file,
	    !,
	    close(S),
	    fail
	  ; X=H ).
