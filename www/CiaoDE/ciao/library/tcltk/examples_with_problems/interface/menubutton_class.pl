%%---------------------------------------------------------------------
%%
%% MENU BUTTON CLASS
%%
%%---------------------------------------------------------------------

:- class(menubutton_class).
:- inherit_class(library('tcltk/examples_with_problems/interface/widget_class')).

:- use_module(library(lists),[append/3]).

:- data        menu/1.
:- inheritable menu/1.

menu('default').

:- export(set_menu/1).

set_menu(Menu) :-
        atom(Menu),
        set_fact(menu(Menu)),
        notify_changes.

:- export([get_menu/1]).

get_menu(Menu) :-
        self(ID),
        owner(OW),
        append([OW],[ID],PP),
%       menu(Menu).
        menu(Menu_name),
        append(PP,[Menu_name],Menu).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

tcl_name(menubutton).

creation_options([' ',min(menu),write(X),write(OW),write(X),write(ID),write(X),C|Other]) :-
        menu(C),
        X='.',
        self(ID),
        owner(OW),
        inherited creation_options(Other).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

menubutton_class.
menubutton_class(Owner) :-
        menubutton_class(Owner).

:- set_prolog_flag(multi_arity_warnings,on).
