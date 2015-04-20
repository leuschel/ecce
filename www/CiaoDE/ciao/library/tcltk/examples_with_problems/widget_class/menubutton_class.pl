%%---------------------------------------------------------------------
%%
%% MENU BUTTON CLASS
%%
%%---------------------------------------------------------------------

:- class(menubutton_class).
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


%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

tcl_name(menubutton).

creation_options([' ',min(menu),br(C)|Other]) :-
        menu(C),
        inherited creation_options(Other).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

menubutton_class.
menubutton_class(Owner) :-
        menubutton_class(Owner).

:- set_prolog_flag(multi_arity_warnings,on).
