:- use_module(library(write)).
:- use_module(library(format)).
:- use_module(library('tcltk/tcltk.pl')).

:- multifile issue_debug_messages/1.
:- data issue_debug_messages/1.
issue_debug_messages('tcltk'). %% remove this line when debug finished


test_command(Command, Result) :-
             tcl_new(Interp),
             tcl_eval(Interp, Command, Result),
             tcl_delete(Interp).

make_assigned_command(Variable,Content,Command) :- Command = ['set',Variable,Content].
make_write_command(Content,Command):- Command = [puts,Content].

test_eval(I) :- 
	tcl_new(I),
	tcl_eval(I,['set x 33'],R1),
	tcl_eval(I,[set,y,33],R2),
	tcl_eval(I,[set,r,'$y+$x'],R3),
	tcl_eval(I,[expr,'$r'],R4),
	tcl_eval(I,[puts,'$r'],R5),
	tcl_delete(I).

	
test2_eval(I):-
	tcl_new(I),
	tcl_eval(I,[label,'.b','-text',dq('Say hello')],R),
	tcl_eval(I,[pack,'.b'],R1),
	tcl_eval(I,[button,'.c','-text',dq('Quit'),'-command',dq('exit')],R2),
	tcl_eval(I,[pack,'.c'],R3),
	tcl_eval(I,[button,'.d','-text',dq('Continue')],R4),
	tcl_eval(I,[pack,'.d'],R5).
%	tcl_delete(I).

test_event:-
	tcl_new(I),
	tcl_event(I,[prolog_one_event,dq(write(execute(hello)))],Event),
	format("Event received 1: ~w",[Event]),
	tcl_eventb(I,[prolog_one__event,dq(write(zap(42)))],Event1),
	format("Event received 2: ~w",[Event1]).
%	tcl_delete(I).

test3_eval(I):-
	tcl_new(I),
	tcl_eval(I,[label,'.l','-text',dq('Select a Potion')],R),
	tcl_eval(I,[set,'l',R],R1),
	tcl_eval(I,[button,'.quitbutton','-text',dq('quit')],R2),
	tcl_eval(I,[set,'quitbutton',R2],R3),
	tcl_eval(I,[radiobutton,'.b0','-text',dq('continue')],R5),
	tcl_eval(I,[radiobutton,'.b1','-text',dq('quit')],R7),
	tcl_eval(I,[radiobutton,'.b2','-text',dq('exit')],R8),
	tcl_eval(I,[pack,'.b0'],R6),
	tcl_eval(I,[pack,'.b1'],R9),
	tcl_eval(I,[pack,'.b2'],R10),
	tcl_eval(I,[pack,'$quitbutton'],R4),
	tcl_eval(I,[place,'$quitbutton','-relx','.01','-rely','.7'],R11).
