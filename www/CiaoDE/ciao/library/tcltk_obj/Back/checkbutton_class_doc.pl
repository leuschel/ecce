%%---------------------------------------------------------------------
%%
%% CHECKBUTTON CLASS
%%
%%---------------------------------------------------------------------

:- module(checkbutton_class_doc,[],[objects,assertions,isomodes,regtypes]).
%:- inherit_class(library('tcltk/examples/interface/widget_class')).



variable('').

:- export(set_variable/1).

%%------------------------------------------------------------------------
:- pred set_variable(+Variable) :: atom
        # "Sets the value of global @var{Variable} to indicate whether or not this button is selected.".
%%------------------------------------------------------------------------
set_variable(Variable) :-
        atom(Variable),
        set_fact(variable(Variable)),
        notify_changes.

:- export([get_variable/1]).

%%------------------------------------------------------------------------
:- pred get_variable(-Variable) :: atom
        # "Gets the value of global @var{Variable} which indicates if the button is selected.".
%%------------------------------------------------------------------------
get_variable(Variable) :-
        variable(Variable).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

:- comment(hide,tcl_name/1).

%%---------------------------------------------------------------------
:- pred tcl_name(-Widget) :: atom 
        # "Specifies the name of the @var{Widget}. In this case is checkbutton.".
%%---------------------------------------------------------------------
tcl_name(checkbutton).

%%---------------------------------------------------------------------
:- pred creation_options(-OptionsList) :: list
        # "Creates a list with the options supported by the checkbutton.".
%%---------------------------------------------------------------------
%creation_options([' -command ',sqb(C)," "|Other]) :-
%creation_options([' -command ',C," "|Other]) :-
creation_options([' ',min(variable),V|Other]) :-
        variable(V),
        creation_options(Other).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------
:- comment(hide,'class$call'/3).
:- comment(hide,'class$used'/2).

:- set_prolog_flag(multi_arity_warnings,off).

checkbutton_class.
checkbutton_class(Owner) :-
        checkbutton_class(Owner).

:- set_prolog_flag(multi_arity_warnings,on).
