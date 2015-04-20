%%---------------------------------------------------------------------
%%
%% RADIOBUTTON CLASS
%%
%%---------------------------------------------------------------------

:- module(radiobutton_class_doc,[],[objects,assertions,isomodes,regtypes]).



variable('').

:- export(set_variable/1).

%%------------------------------------------------------------------------
:- pred set_variable(+Variable) :: atom
        # "Specifies the value of global @var{Variable} to set whenever this button is selected.".
%%------------------------------------------------------------------------
set_variable(Variable) :-
        atom(Variable),
        set_fact(variable(Variable)),
        notify_changes.

:- export([get_variable/1]).

%%------------------------------------------------------------------------
:- pred get_variable(-Variable) :: atom
        # "Gets the value of global @var{Variable} which indicates if this button is selected.".
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
        # "Specifies the name of the @var{Widget}. In this case is radiobutton.".
%%---------------------------------------------------------------------
tcl_name(radiobutton).

%%---------------------------------------------------------------------
:- pred creation_options(-OptionsList) :: list
        # "Creates a list with the options supported by the radiobutton.".
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

radiobutton_class.
radiobutton_class(Owner) :-
        radiobutton_class(Owner).

:- set_prolog_flag(multi_arity_warnings,on).
