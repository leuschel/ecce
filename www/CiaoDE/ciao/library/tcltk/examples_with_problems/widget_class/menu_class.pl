%%---------------------------------------------------------------------
%%
%% MENU CLASS
%%
%%---------------------------------------------------------------------

:- class(menu_class).

:- inherit_class(library('tcltk/examples/widget_class/widget_class')).

:- data        menu/1.
:- inheritable menu/1.

menu('').

:- export(set_menu/1).

set_menu(Menu) :-
        atom(Menu),
        set_fact(menu(Menu)),
        notify_changes.

:- export([get_menu/1]).

get_menu(Menu) :-
        menu(Menu).

:- data        label/1.
:- inheritable label/1.

label('').

:- export(set_label/1).

set_label(Label) :-
        atom(Label),
        set_fact(label(Label)),
        notify_changes.

:- export([get_label/1]).

get_label(Label) :-
        label(Label).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

tcl_name('').

%creation_options([write('.'),write(Widget),write('.'),write(C),' add command',min(label),label(L)|Other]) :-
creation_options([write('.'),write(Widget),' add command',min(label),label(L)|Other]) :-
        self(Widget),
        menu(C),
        label(L),
        inherited creation_options(Other).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

menu_class.
menu_class(Owner) :-
        menu_class(Owner).

:- set_prolog_flag(multi_arity_warnings,on).
