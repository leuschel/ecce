:- module(_, _, []).

:- use_module(library(sockets)).
:- use_module(library(random)).

main :-
	random(S),
%	connect_to_socket(localhost, 88, S),
	display(S), nl.
