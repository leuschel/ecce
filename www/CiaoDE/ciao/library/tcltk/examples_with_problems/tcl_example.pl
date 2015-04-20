:- module(tcl_example,[test/0]).

%:- use_module(library('tcltk/examples/tk_test_aux')).

:- use_module(library(tcltk)).
:- use_module(library(lists)).
:- use_module(library(format)).
%:- use_module(library(atom2term)).


test :-
	tcl_new(X),
	test_aux(X).

test_aux(X) :-
%	tcl_eval(X,[button,'.b','-text',dq('Say hello')],_),
	tcl_eval(X,[button,'.b',min(text),dq('Say hello')],_),
	tcl_eval(X,[button,'.c','-text',dq('Quit')],_),
	tcl_eval(X,[pack,'.b','.c'],_),
	tcl_eval(X,[bind,'.b','<ButtonPress-1>',
                    br([prolog_one_event,dq(write(execute(hello)))])],_),
	tcl_eval(X,[bind,'.c','<ButtonPress-1>',
	            br([prolog_one_event,dq(write(exit))])],_),
	event_loop(X).

test_aux(X) :-
	tcl_delete(X).


event_loop(X) :-
	tcl_event(X,[],Event),
	(  member(execute(Goal),Event) 
	-> call(Goal), event_loop(X) 
	;  tcl_delete(X) ).

%hello :-
%	display('Hello !!!!'),
%	nl.
