
:- module(tk_options2,[test/0]).

:- use_module(library('tcltk/tcltk')). 
:- use_module(library('tcltk/examples/tk_test_aux')). 
:- use_module(library(aggregates)).

% Example to probe the options of the tk_new predicate
% You could change the enviroment variable DISPLAY
test:-
%	tk_new([name('pepe'),display('r2d3:0.0')],Interp),
	tk_new([display('r2d3:0.0')],Interp),
	test_aux(Interp).

test_aux(Interp):-
	tcl_eval(Interp,'button .b -text "Say Hello"',_),
	tcl_eval(Interp,[button,'.c','-text',dq('Quit')],_),
	tcl_eval(Interp,'pack .b .c',_),
	tcl_eval(Interp,[bind,'.b','<ButtonPress-1>',
                    br([prolog_one_event,dq(write(execute(tk_test_aux:hello)))])],_),
	tcl_eval(Interp,[bind,'.c','<ButtonPress-1>',
	            br([prolog_one_event,dq(write(exit))])],_),
        tk_event_loop(Interp),
	tcl_delete(Interp).
