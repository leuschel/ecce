:- module(widget_event,[test/0],[objects]).

:- use_module(library(system),[pause/1]).
:- use_module(library('tcltk/examples/tk_test_aux')).

:- use_class(library('tcltk/examples/widget_class/window_class')).
:- use_class(library('tcltk/examples/widget_class/widget_class')).
:- use_class(library('tcltk/examples/widget_class/button_class')).
:- use_class(library('tcltk/examples/widget_class/label_class')).
:- use_class(library('tcltk/examples/widget_class/entry_class')).


test :- 
        B1 new button_class,
        B1:set_text('Say Hello'),
        B1:set_font('times'),
        B1:set_highlightbackground('green'),    
        B1:set_highlightcolor('blue'),
        B1:set_event_type('<ButtonPress-1>'),
        B1:set_predicate('tk_test_aux:hello'),
        B1:set_relief('raised'),

        B1:get_font(X),
        


        C2 new button_class,
        C2:set_text('Quit'),
        C2:set_font(X),
        C2:set_event_type('<ButtonPress-1>'),
        C2:set_predicate('tk_test_aux:quit'),
        C2:set_relief('raised'),

        W1 new window_class([B1,C2]),
        W1:show,

        W1:event_loop,

%       hit_enter,

        destroy W1.

hit_enter :-
        display('Hit ENTER to continue...'),
        nl,
        get_code(_).
