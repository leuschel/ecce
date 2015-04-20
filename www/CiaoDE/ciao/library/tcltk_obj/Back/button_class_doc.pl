%%---------------------------------------------------------------------
%%
%% BUTTON CLASS
%%
%%---------------------------------------------------------------------

:- module(button_class_doc,[],[objects,assertions,isomodes,regtypes]).
:- use_module(library('tcltk_obj/widget_class')).


command('').

:- export(set_command/1).

%%------------------------------------------------------------------------
:- pred set_command(+Command) :: atom
        # "Sets a Tcl @var{Command} to be associated with the button. This @var{Command} is typically invoked when mouse button 1 is released over the button window.".
%%------------------------------------------------------------------------
set_command(Command) :-
        atom(Command),
        set_fact(command([prolog_one_event,dq(write(execute(Command)))])),
        notify_changes.

:- export([get_command/1]).

%%------------------------------------------------------------------------
:- pred get_command(-Command) :: atom
        # "Gets the Tcl @var{Command} associated with the button.".
%%------------------------------------------------------------------------
get_command(Command) :-
        command(Command).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

:- comment(hide,tcl_name/1).

%%---------------------------------------------------------------------
:- pred tcl_name(-Widget) :: atom 
        # "Specifies the name of the @var{Widget}. In this case is button.".
%%---------------------------------------------------------------------
tcl_name(button).

%%---------------------------------------------------------------------
:- pred creation_options(-OptionsList) :: list
        # "Creates a list with the options supported by the button.".
%%---------------------------------------------------------------------
%creation_options([' -command ',sqb(C)," "|Other]) :-
%creation_options([' -command ',C," "|Other]) :-
creation_options([' ',min(command),br(C)|Other]) :-
        self(ID), display(ID),nl,
        command(C),
        creation_options(Other).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------
:- comment(hide,'class$call'/3).
:- comment(hide,'class$used'/2).

:- set_prolog_flag(multi_arity_warnings,off).

button_class.
button_class(Owner) :-
        button_class(Owner).

:- set_prolog_flag(multi_arity_warnings,on).
