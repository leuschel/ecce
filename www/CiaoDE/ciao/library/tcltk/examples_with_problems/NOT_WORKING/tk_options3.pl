:- module(tk_options3,[test/0]).

:- use_module(library('tcltk/tcltk')). 
:- use_module(library('tcltk/examples/tk_test_aux')). 
:- use_module(library(aggregates)).

%Example to probe the options of tk_new predicate
%You could open a tcltk script.Y

test:-
	tk_new([file('tk_example_file.tcl')],Interp).
	tcl_delete(Interp).
