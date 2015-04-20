%%---------------------------------------------------------------------
%%
%% MENU BUTTON CLASS
%%
%%---------------------------------------------------------------------

:- module(menubutton_class_doc,[],[objects,assertions,isomodes,regtypes]).
%:- inherit_class(library('tcltk/examples/interface/widget_class')).

:- use_module(library(lists),[append/3]).


menu('default').

:- export(set_menu/1).

%%------------------------------------------------------------------------
:- pred set_menu(+Menu) :: atom
        # "@var{Menu} posted when menubutton is clicked.".
%%------------------------------------------------------------------------
set_menu(Menu) :-
        atom(Menu),
        set_fact(menu(Menu)),
        notify_changes.

:- export([get_menu/1]).

%%------------------------------------------------------------------------
:- pred get_menu(-Menu) :: atom
        # "Gets the name of the @var{Menu} asociated to the menubutton.".
%%------------------------------------------------------------------------
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

:- comment(hide,tcl_name/1).
%%---------------------------------------------------------------------
:- pred tcl_name(-Widget) :: atom 
        # "Specifies the name of the @var{Widget}. In this case is menubutton.".
%%---------------------------------------------------------------------
tcl_name(menubutton).

%%---------------------------------------------------------------------
:- pred creation_options(-OptionsList) :: list
        # "Creates a list with the options supported by the menubutton.".
%%---------------------------------------------------------------------
creation_options([' ',min(menu),write(X),write(OW),write(X),write(ID),write(X),C|Other]) :-
        menu(C),
        X='.',
        self(ID),
        owner(OW),
        creation_options(Other).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- comment(hide,'class$call'/3).
:- comment(hide,'class$used'/2).

:- set_prolog_flag(multi_arity_warnings,off).

menubutton_class.
menubutton_class(Owner) :-
        menubutton_class(Owner).

:- set_prolog_flag(multi_arity_warnings,on).
