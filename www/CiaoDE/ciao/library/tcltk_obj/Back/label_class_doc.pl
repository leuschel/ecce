%%---------------------------------------------------------------------
%%
%% LABEL CLASS
%%
%%---------------------------------------------------------------------

:- module(label_class_doc,[],[objects,assertions,isomodes,regtypes]).

%:- inherit_class(library('tcltk/examples/interface/widget_class')).


textvariable('').

:- export(set_textvariable/1).

%%---------------------------------------------------------------------
:- pred set_textvariable(+Variable) :: atom
        # "@var{Variable} specifies name of the Tcl variable".
%%---------------------------------------------------------------------
set_textvariable(Textvariable) :-
        atom(Textvariable),
        set_fact(textvariable(Textvariable)),
        notify_changes.

:- export([get_textvariable/1]).

%%---------------------------------------------------------------------
:- pred get_textvariable(-Value) :: num
        # "@var{Value} is the value of the Tcl variable associated to the label.".
%%---------------------------------------------------------------------
get_textvariable(Textvariable) :-
        textvariable(Textvariable).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

:- comment(hide,tcl_name/1).

%%---------------------------------------------------------------------
:- pred tcl_name(-Widget) :: atom 
        # "Specifies the name of the @var{Widget}. In this case is label.".
%%---------------------------------------------------------------------
tcl_name(label).

%%---------------------------------------------------------------------
:- pred creation_options(-OptionsList) :: list
        # "Creates a list with the options supported by the label.".
%%---------------------------------------------------------------------
creation_options([''|Other]) :-
%       textvariable(T),
        creation_options(Other).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------
:- comment(hide,'class$call'/3).
:- comment(hide,'class$used'/2).

:- set_prolog_flag(multi_arity_warnings,off).

label_class.
label_class(Owner) :-
        label_class(Owner).


:- set_prolog_flag(multi_arity_warnings,on).
