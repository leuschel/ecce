:- module(_, _, [macro]).

% Try to comment or uncomment the following lines:
:- define(use_write).
:- define(use_quote).

:- if(use_write).
:-   use_module(library(write)).
:-   if(use_quote).
pr(X) :- writeq(using_write(X)), nl.
:-   else.
pr(X) :- write(using_write(X)), nl.
:-   fi.
:- else.
:-   if(use_quote).
pr(X) :- displayq(using_display(X)), nl.
:-   else.
pr(X) :- display(using_display(X)), nl.
:-   fi.
:- fi.

main :-
	pr('hello world').
