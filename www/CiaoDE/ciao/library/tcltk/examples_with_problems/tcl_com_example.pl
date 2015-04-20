%%-----------------------------------------------------------------------
%%  Test tcl_command
%%
%% Montse Iglesias
%% Example to prove tcl_command.pl 
%%-----------------------------------------------------------------------

:- module(tcl_com_example,[test/0]).

:- use_module(library('tcltk/tcl_command')).
:- use_module(library('tcltk/tcltk')).


test :-
	tcl_new(X),
	make_button_command('b','Say Hello',C),
	make_button_command('c','Quit',C2),
	make_label_command('d','data',C6),
	make_pack_command('b',C1),
	make_pack_command('c',C3),
	make_pack_command('d',C7),
	make_bind_command('b','ButtonPress-1','hello',C4),
	make_bind_command('c','ButtonPress-1','quit',C5),
	test_command(X,[C,C2,C6,C1,C3, C7,C4,C5]),
	tk_event_loop(X).

hello :-
	display('Hello !!!!'),
	nl.
