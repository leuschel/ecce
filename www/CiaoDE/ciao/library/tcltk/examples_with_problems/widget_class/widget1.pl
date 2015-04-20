:- module(widget1,[test/0],[objects]).

:- use_module(library(system),[pause/1]).
:- use_module(library('tcltk/examples/tk_test_aux')).

:- use_class(library('tcltk/examples/widget_class/window_class')).
:- use_class(library('tcltk/examples/widget_class/widget_class')).
:- use_class(library('tcltk/examples/widget_class/button_class')).
:- use_class(library('tcltk/examples/widget_class/label_class')).
:- use_class(library('tcltk/examples/widget_class/entry_class')).


test :- 
        E1 new entry_class,
%       E1:set_justify('right'),
        E1:set_background('white'),
        E1:set_borderwidth('2'),
        E1:set_foreground('black'),
%       E1:set_side('left'),
        E1:set_expand('1'),
%       E1:set_fill('both'),
        E1:set_padx('8'),
        E1:set_pady('8'),
        E1:set_textvariable('inputval'),

%       E1:get_textvariable(Z),

        B1 new button_class,
        B1:set_text('Factorial'),
        B1:set_font('times'),
%       B1:set_highlightbackground('green'),    
%       B1:set_highlightcolor('blue'),
%       B1:set_event_type('<ButtonPress-1>'),
%atom_concat('tk_test_aux:factorial(','$inputval',V1),
%display(V1),
%atom_concat(V1,',Outputval)',V2),
%display(V2),
%       B1:set_predicate('tk_test_aux:factorial($inputval,Outputval)'),
%       B1:set_predicate(V2),
        B1:set_relief('raised'),
        B1:set_side('left'),


%       result(Y),

        B1:get_font(X),
        

        C2 new button_class,
        C2:set_text('Quit'),
        C2:set_font(X),
        C2:set_command('tk_test_aux:quit'),
%       C2:set_event_type('<ButtonPress-1>'),
%       C2:set_predicate('tk_test_aux:quit'),
        C2:set_relief('raised'),


        X2 new entry_class,
        X2:set_background('white'),
        X2:set_textvariable('Outputval'),
 
        L1 new label_class,
%       L1:set_text(Z),
        L1:set_width('0'),

atom_concat('tk_test_aux:factorial(','$inputval',V1),
atom_concat(V1,',Outputval)',V2),
%       B1:set_predicate('tk_test_aux:factorial($inputval,Outputval)'),
        B1:set_command(V2),


        W1 new window_class([B1]),
        W1:show,

%       W1:set_variable(Z),
        W1:event_loop,

        hit_enter,

        destroy W1.

hit_enter :-
        display('Hit ENTER to continue...'),
        nl,
        get_code(_).

result(Y) :-
        Y = 'Hola!!!'.
