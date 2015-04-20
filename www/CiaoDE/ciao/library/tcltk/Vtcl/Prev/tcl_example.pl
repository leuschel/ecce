:- module(tcl_example,[test/0]).

:- use_module(library(tcltk)).
:- use_module(library(lists)).
:- use_module(library(format)).

test :-
	tcl_new(X),
	tcl_eval(X,[button,'.b','-text',dq('Say hello')],_),
	tcl_eval(X,[button,'.c','-text',dq('Quit')],_),
	tcl_eval(X,[pack,'.b','.c'],_),
	tcl_eval(X,[bind,'.b','<ButtonPress>',
                    br([prolog_event,br([dq(write(execute(hello)))])])],_),
%% 	tcl_eval(X,[bind,'.c','<ButtonPress>',
%%                     br([prolog_event,br([dq(write(quit))])])],_),
	event_loop(X).

event_loop(X) :-
	format("Entering event loop~n",[]),
	tcl_event(X,[],Event),
	format("Event received: ~s~n",[Event]),
	(  member(execute(Goal),Event) 
	-> call(Goal), event_loop(X) 
	;  tcl_delete(X) ).

hello :-
	display('Hello !!!!'),
	nl.
