%%----------------------------------------------------------------------
%%
%% O'CIAO
%%
%% PERSISTENT OBJECT EXAMPLE
%%
%%----------------------------------------------------------------------

:- module(persistency_example,[],[objects]).

%%----------------------------------------------------------------------

:- use_class(library('class/library/file_persistent_streamer')).
:- use_class(library('class/examples/persistency/persistent_class_example')).

:- use_module(library(read)).

%%----------------------------------------------------------------------

:- persistent_object instance_of 
	persistent_class_example(
		file_persistent_streamer('./persistent_object')	).

%%----------------------------------------------------------------------

:- export(main/0).

main :-
	current_state,
	add_state,
	destroy(persistent_class_example(persistent_object)),
	nl,nl.

current_state :-
	persistent_object:count(N),
	inform_user(['CURRENT STATE: (',N,' items)']),nl,
	persistent_object:get(I),
	display(I),nl,
	fail.
current_state :-
	nl.

add_state :-
	inform_user(['TYPE NEW TERM FOR OBJECT STORAGE: (Ctrl-D to exit)']),
	read_term(T,[]),
	T \== end_of_file,
	!,
	persistent_object:add(T),
	add_state.
add_state.
