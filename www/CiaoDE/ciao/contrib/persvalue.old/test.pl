:- module(test,_,[persdb]).
:- use_module(library(persvalue)).

:- initialization(defaults).

%:- persistent(persvalue/2, dbdir).
%:- persistent(default_value/2, dbdir).

:- data pers_value_tmp/2.

defaults :-
	configure_value(color_objects,  tree, green, 'What is the color of the tree?'),
	configure_value(color_objects, apple,   red, 'What is the color of the apple?'),
	configure_value(color_objects,   sky,  blue, 'What is the color of the sky?').

main :-
	query_opts([color_objects]).
main.
