
:- module(tk_options,[test/0]).

:- use_module(library('tcltk/tcltk')). 
:- use_module(tk_test_aux). 
:- use_module(library(aggregates)).

% Example to probe one of the options of predicate tk_new.
% You could change the name of the window.
 
test:-
	tk_new([name('pepe')],Interp),
	test_aux(Interp).

test_aux(Interp):-
	tcl_eval(Interp,'button .b -text "Say Hello"',_),
	tcl_eval(Interp,[button,'.c','-text',dq('Quit')],_),
	tcl_eval(Interp,'pack .b .c',_),
	tcl_eval(Interp,[bind,'.b','<ButtonPress-1>',
                    br([prolog_one_event,dq(write(execute(tk_test_aux:hello)))])],_),
	tcl_eval(Interp,[bind,'.c','<ButtonPress-1>',
	            br([prolog_one_event,dq(write(quit))])],_),
        tk_event_loop(Interp),
	tcl_delete(Interp).
