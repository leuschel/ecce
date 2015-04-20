
:- module(tk_options4,[test/0]).

:- use_module(library('tcltk/tcltk')). 
:- use_module(library('tcltk/examples/tk_test_aux')). 
:- use_module(library(aggregates)).

test:-
	tcl_new(Interp),
	tcl_eval(Interp,[source,'tk_example_file.tcl'],_),
        tk_event_loop(Interp).
