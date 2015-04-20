%%---------------------------------------------------------------------
%%
%% BUTTON CLASS
%%
%%---------------------------------------------------------------------

:- class(button_class).
:- inherit_class(library('tcltk_obj/widget_class')).

:- data        command/1.
:- inheritable command/1.

command('').

:- export(set_command/1).

set_command(Command) :-
	atom(Command),
	set_fact(command([prolog_one_event,dq(write(execute(Command)))])),
	notify_changes.

:- export([get_command/1]).

get_command(Command) :-
	command(Command).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

tcl_name(button).

%creation_options([' -command ',sqb(C)," "|Other]) :-
%creation_options([' -command ',C," "|Other]) :-
creation_options([' ',min(command),br(C)|Other]) :-
%	self(ID), 
	command(C),
	inherited creation_options(Other).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

button_class.
button_class(Owner) :-
	button_class(Owner).

:- set_prolog_flag(multi_arity_warnings,on).
