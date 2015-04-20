:- module(tcl_thr_example,[test/0]).

:- use_module(library(tcltk)).
:- use_module(library(lists)).
:- use_module(library(concurrency)).

test :-
	tcl_new(X),
	tcl_eval(X,[button,'.b','-text',dq('Say hello')],_),
	tcl_eval(X,[button,'.c','-text',dq('Quit')],_),
	tcl_eval(X,[pack,'.b','.c'],_),
	tcl_eval(X,[bind,'.b','<ButtonPress>',
                    br([prolog_one_event,dq(write(execute(hello)))])],_),
	tcl_eval(X,[bind,'.c','<ButtonPress>',
                    br([prolog_one_event,dq(write(quit))])],_),
	eng_call(event_loop(X),create,create).

event_loop(X) :-
	tcl_event(X,[],Event),
	(  member(execute(Goal),Event) 
	-> call(Goal), event_loop(X) 
	;  tcl_delete(X) ).

hello :-
	display('Hello !!!!'),
	nl.
