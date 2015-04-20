:- module(configutils, [get_string/1, get_string/2, get_atom/1,
   get_atom/2], [iso, functions]).

:- use_module(library(system)).
:- use_module(library('make/system_extra')).

:- use_module(library(persvalue)).

get_current_value(G, X, Y) :-
	current_fact(current_value(G, X, Y)) -> true
 ;
	message(error,[G, ': Could not get value ', Y, ' for name ', X]),
	fail.

get_atom(Atom) :-
	current_input(S),
	get_atom(S, Atom).
get_atom(Stream, Atom) :-
	get_string(Stream, String),
	atom_codes(Atom, String).

get_string(Stream, String) :-
	get_code(Stream, Code),
	(   "\n" = [Code] ->
	    String = []
	;
	    String = [Code|String2],
	    get_string(Stream, String2)
	).
get_string(String) :-
	current_input(S),
	get_string(S, String).
