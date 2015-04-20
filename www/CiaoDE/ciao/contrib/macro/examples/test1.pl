:- module(_, _, [macro]).

% Try to comment or uncomment the following line:
:- define(use_write).

:- if(use_write).
:- use_module(library(write)).
pr(X) :-
	write(using_write(X)), nl.
:- else.
pr(X) :-
	display(using_display(X)), nl.
:- fi.

main :-
	pr('hello world').
