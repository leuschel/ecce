
:- module(tk_test,[test/0]).

:- use_module(library('tcltk/tcltk')). 
:- use_module(library('tcltk/examples/tk_test_aux')). 
:- use_module(library(aggregates)).
:- use_module(library(strings)).

:- export(test_aux/1).

test:-
	tcl_new(Interp),
	test_aux(Interp).

test_aux(Interp):-
	tcl_eval(Interp,[entry, '.e1' ,'-textvariable', 'inputval'],_),
	tcl_eval(Interp,[set, 'outputval' ,'_'],_),
	tcl_eval(Interp,[button,'.b1', '-text',dq('Factorial'), '-command',
           br([prolog_one_event,dq(write(execute(tk_test_aux:factorial('$inputval','$outputval'))))])],_),
	tcl_eval(Interp,'pack .e1',_),
	tcl_eval(Interp,'pack .b1',_),
        tk_event_loop(Interp),
	tcl_delete(Interp). 
