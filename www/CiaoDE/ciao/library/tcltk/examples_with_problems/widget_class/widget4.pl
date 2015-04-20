:- module(widget4,[test/0],[objects]).

:- use_module(library(tcltk)).


:- use_class(library('tcltk/examples_with_problems/widget_class/window_class')).
:- use_class(library('tcltk/examples_with_problems/widget_class/widget_class')).
:- use_class(library('tcltk/examples_with_problems/widget_class/button_class')).
:- use_class(library('tcltk/examples_with_problems/widget_class/label_class')).
:- use_class(library('tcltk/examples_with_problems/widget_class/entry_class')).


:- b1 instance_of button_class.
:- w1 instance_of window_class(
        [
        button_class(b1)
        ]).

test :- 
        button_class(b1):set_text('Say Hello'),
        button_class(b1):set_font('times'),
        button_class(b1):set_relief('raised'),
        button_class(b1):set_event_type('<ButtonPress-1>'),
        button_class(b1):set_action('widget4:hello'),

        window_class(w1):show,
        window_class(w1):event_loop.


hit_enter :-
        display('Hit ENTER to continue...'),
        nl,
        get_code(_).



hello :- 
        button_class(b1):set_font('arial'),
        window_class(w1):show,
        display('Hello!!!!'),
        nl.
