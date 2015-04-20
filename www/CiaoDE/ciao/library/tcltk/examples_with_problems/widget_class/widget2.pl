:- module(widget2,[test/0],[objects]).

:- use_module(library(system),[pause/1]).
:- use_module(library('tcltk/examples/tk_test_aux')).
:- use_module(library(tcltk)).

:- use_class(library('tcltk/examples/widget_class/window_class')).
:- use_class(library('tcltk/examples/widget_class/widget_class')).
:- use_class(library('tcltk/examples/widget_class/button_class')).
:- use_class(library('tcltk/examples/widget_class/label_class')).
:- use_class(library('tcltk/examples/widget_class/entry_class')).

:- e1 instance_of entry_class.
:- b1 instance_of button_class.
:- w1 instance_of window_class(
        [
        entry_class(e1),
        button_class(b1)
        ]).

test :- 
%       B1 new button_class,
%       set_fact(button1(B1)),
        b1:set_text('Factorial'),
        b1:set_font('times'),
        b1:set_relief('raised'),
        b1:set_side('left'),
%       B1:get_font(X),
        b1:set_event_type('<ButtonPress-1>'),
%       B1:set_action('inputval','Outputval','tk_test_aux:factorial($inputval,Outputval)'),
%       B1:set_action('inputval','Outputval','tk_test_aux:factorial($inputval,Outputval)'),
        b1:set_action('widget2:factorial'),
        b1:set_variables('inputval'),

%       display(B1),

%       E1 new entry_class,
%       set_fact(entry1(E1)),
%       e1:set_justify('right'),
%       e1:set_background('white'),
%       e1:set_borderwidth('2'),
%       e1:set_foreground('black'),
%       e1:set_side('left'),
%       e1:set_expand('1'),
%       e1:set_fill('both'),
%       e1:set_padx('8'),
%       e1:set_pady('8'),
%       e1:set_textvariable('inputval'),

%       E1:get_textvariable(X),
%       E1:set_event_type('<Any-KeyPress>'),
%       E1:set_action('get_value('$inputval','),

%       X2 new entry_class,
%       set_fact(entry2(X2)),
%       X2:set_background('white'),
%       X2:set_textvariable('Outputval'),

%       C2 new button_class,
%       C2:set_text('Quit'),
%       C2:set_font(X),
%       C2:set_command('tk_test_aux:quit'),
%       C2:set_event_type('<ButtonPress-1>'),
%       C2:set_action('tk_test_aux:quit'),
%       C2:set_relief('raised'),

%       E1:get_textvariable(Z),

%       W1 new window_class([e1,B1,C2,X2]),
%        display(1),nl,
        w1:show,
%       set_fact(window1(W1)),
%       W1:interp(I1),
%        W1:get_variable(Y,X),
        w1:event_loop,

        hit_enter,
        destroy B1,
        hit_enter,
        destroy w1,
true.

hit_enter :-
        display('Hit ENTER to continue...'),
        nl,
        get_code(_).

%factorial :- 
%       display(e1),nl,
%       entry1(E1),
%        e1:get_textvariable(Y),
%       display(Y),
%       nl,
%       window1(W1),
%       display('Factorial'),nl,
%        W1:get_variable(Y,X),
%       display('La variable'),nl,
%       display(X),
%       nl.
%       tk_test_aux:factorial(X,Z),
%       entry2(E2),
%       E2:set_textvariable(X),
%       display(X),
%       nl.
