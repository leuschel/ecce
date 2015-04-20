:- module(widget3,[test/0],[objects]).

:- use_module(library('tcltk/examples/tk_test_aux')).
%:- use_module(library(tcltk)).


:- use_class(library('tcltk/examples_with_problems/interface/window_class')).
:- use_class(library('tcltk/examples_with_problems/interface/widget_class')).
:- use_class(library('tcltk/examples_with_problems/interface/button_class')).
:- use_class(library('tcltk/examples_with_problems/interface/label_class')).
:- use_class(library('tcltk/examples_with_problems/interface/entry_class')).
:- use_class(library('tcltk/examples_with_problems/interface/menubutton_class')).
:- use_class(library('tcltk/examples_with_problems/interface/menu_class')).
:- use_class(library('tcltk/examples_with_problems/interface/menu_entry_class')).


:- e1 instance_of entry_class.
:- e2 instance_of entry_class.
:- b1 instance_of button_class.
:- b2 instance_of button_class.
:- l1 instance_of label_class.
:- l2 instance_of label_class.
:- b3 instance_of button_class.
:- b4 instance_of button_class.
:- l3 instance_of label_class.
:- l4 instance_of label_class.
:- m1 instance_of menubutton_class.
:- me1 instance_of menu_entry_class.
:- me2 instance_of menu_entry_class.
:- m2 instance_of menu_class(
        [
        menu_entry_class(me1),
        menu_entry_class(me2)]).
:- w2 instance_of window_class(
        [
        label_class(l3),
        button_class(b3),
        button_class(b4)
        ],[]).
:- w1 instance_of window_class(
        [
        label_class(l1),
        entry_class(e1),
        label_class(l2),
        entry_class(e2),
        button_class(b1),
        button_class(b2)
        ],[]).

:- w0 instance_of window_class(
        [
        menubutton_class(m1),
        label_class(l4)
        ],[menu_class(m2)]).

presentation :- 
        window_class(w0):set_withdraw,
        button_class(b1):set_text('Factorial'),
        button_class(b1):set_font('arial'),
        button_class(b1):set_relief('raised'),
        button_class(b1):set_side('left'),
        button_class(b1):set_event_type('<ButtonPress-1>'),
        button_class(b1):set_action('widget3:factorial'),
        button_class(b1):set_variables('inputval'),
        button_class(b1):set_row('4'),
        button_class(b1):set_column('0'),

        entry_class(e1):set_justify('right'),
        entry_class(e1):set_background('white'),
        entry_class(e1):set_borderwidth('2'),
        entry_class(e1):set_foreground('black'),
        entry_class(e1):set_expand('1'),
        entry_class(e1):set_fill('both'),
%       entry_class(e1):set_padx('8'),
%       entry_class(e1):set_pady('8'),
        entry_class(e1):set_textvariable('inputval'),
        entry_class(e1):set_font('arial'),
        entry_class(e1):set_row('1'),
        entry_class(e1):set_column('0'),
        entry_class(e1):set_columnspawn('2'),

        entry_class(e2):set_justify('right'),
        entry_class(e2):set_background('white'),
        entry_class(e2):set_borderwidth('2'),
        entry_class(e2):set_foreground('black'),
        entry_class(e2):set_expand('1'),
        entry_class(e2):set_fill('both'),
        entry_class(e2):set_textvariable('Outputval'),
        entry_class(e2):set_font('arial'),
        entry_class(e2):set_row('3'),
        entry_class(e2):set_column('0'),
        entry_class(e2):set_columnspawn('2'),
 
        button_class(b2):set_text('Quit'),
        button_class(b2):set_font('arial'),
        button_class(b2):set_event_type('<ButtonPress-1>'),
        button_class(b2):set_action('widget3:quit'),
        button_class(b2):set_relief('raised'),
        button_class(b2):set_side('right'),
        button_class(b2):set_row('4'),
        button_class(b2):set_column('2'),

        label_class(l1):set_text('El factorial de : '),
        label_class(l1):set_font('arial'),
        label_class(l1):set_relief('flat'),
        label_class(l1):set_background('gray'),
        label_class(l1):set_row('0'),
        label_class(l1):set_column('0'),
        label_class(l1):set_rowspawn('2'),

        label_class(l2):set_text('es : '),
        label_class(l2):set_font('arial'),
        label_class(l2):set_relief('flat'),
        label_class(l2):set_row('2'),
        label_class(l2):set_column('0'),
        label_class(l2):set_rowspawn('2'),

        window_class(w1):show,
        window_class(w1):set_title("Factorial"),
        window_class(w1):set_maxsize(300,300),
        window_class(w1):set_minsize(200,200),
        window_class(w1):event_loop,

        destroy window_class(w1).


test :-
        label_class(l4):set_text('FACTORIAL'),
        label_class(l4):set_font('arial'),
        label_class(l4):set_relief('flat'),
        label_class(l4):set_background('gray'),
        label_class(l4):set_row('5'),
        label_class(l4):set_column('0'),
        label_class(l4):set_columnspawn('2'),

        menubutton_class(m1):set_menu('m2'),
        menubutton_class(m1):set_text('Options'),
        menubutton_class(m1):set_relief('raised'),
        menubutton_class(m1):set_side('top'),
        menubutton_class(m1):set_font('Times'),
        menubutton_class(m1):get_menu(Y),
%        menubutton_class(m1):set_row('0'),
%        menubutton_class(m1):set_column('0'),
        menubutton_class(m1):set_columnspawn('2'),

        menu_class(m2):set_tearoff('1'),
        menu_class(m2):set_name(Y),

        menu_entry_class(me1):set_name(Y),
        menu_entry_class(me1):set_label('Factorial'),
        menu_entry_class(me1):set_action('widget3:presentation'),

        menu_entry_class(me2):set_name(Y),
        menu_entry_class(me2):set_label('Quit'),
        menu_entry_class(me2):set_action('widget3:quit'),

        window_class(w0):show,
        window_class(w0):set_title("Factorial"),
        menu_class(m2):show,
        window_class(w0):event_loop,
        destroy window_class(w0).

factorial :- 
        entry_class(e1):get_textvariablevalue(X),
        tk_test_aux:factorial(X,Z),
        entry_class(e2):set_textvariablevalue(Z).
        
quit :- 
        window_class(w1):set_withdraw,
        window_class(w0):set_withdraw,
        label_class(l3):set_text('Do you want to quit? '),
        label_class(l3):set_font('arial'),
        label_class(l3):set_relief('flat'),
        label_class(l3):set_background('gray'),
        button_class(b3):set_columnspawn('0'),

        button_class(b3):set_text('Ok'),
        button_class(b3):set_font('arial'),
        button_class(b3):set_event_type('<ButtonPress-1>'),
        button_class(b3):set_action('tk_test_aux:quit'),
        button_class(b3):set_relief('raised'),
        button_class(b3):set_row('1'),
        button_class(b3):set_column('0'),

        button_class(b4):set_text('Cancel'),
        button_class(b4):set_font('arial'),
        button_class(b4):set_event_type('<ButtonPress-1>'),
        button_class(b4):set_action('widget3:presentation'),
        button_class(b4):set_relief('raised'),
        button_class(b4):set_row('1'),
        button_class(b4):set_column('2'),

        entry_class(e2):set_background('green'),

        window_class(w2):show,
        window_class(w2):set_title("Confirm"),
        window_class(w2):event_loop,
        destroy window_class(w2).
