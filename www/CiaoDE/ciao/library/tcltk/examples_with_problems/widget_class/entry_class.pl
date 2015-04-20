%%---------------------------------------------------------------------
%%
%% ENTRY CLASS
%%
%%---------------------------------------------------------------------

:- class(entry_class).
:- use_module(library(lists),[append/3]).
:- inherit_class(library('tcltk/examples_with_problems/widget_class/widget_class')).


:- data        textvariable/1.
:- inheritable textvariable/1.

textvariable('aux').

:- export(set_textvariable/1).

set_textvariable(Textvariable) :-
        atom(Textvariable),
        set_fact(textvariable(Textvariable)),
        display('antes notify_changes'),nl,
        notify_changes.

:- export([get_textvariablevalue/1]).

get_textvariablevalue(Textvariable) :-
        textvariable(Textvariable_aux),
        atom_concat('$',Textvariable_aux,Textvariable).

:- export([get_textvariable/1]).

get_textvariable(Textvariable) :-
        textvariable(Textvariable).

:- data        justify/1.
:- inheritable justify/1.

justify('left').

:- export(set_justify/1).

set_justify(Side) :-
        atom(Side),
        set_fact(justify(Side)),
        notify_changes.

:- export([get_justify/1]).

get_justify(Side) :-
        justify(Side).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1,creation_bind/1]).

tcl_name(entry).

%creation_options([' ',min(justify),S,min(textvariable),T,''|Other]) :-
creation_options([' ',min(justify),S,''|Other1]) :-
        justify(S),
        textvariable(T),
        inherited creation_options(Other),
        append(Other,[min(textvariable)|write(T)],Other1).

%creation_bind([' ','<Any-Key>',br([prolog_one_event,dq(write(execute(widget3:anadir(inputval(7)))))])]) .


%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

entry_class.
entry_class(Owner) :-
        entry_class(Owner).


:- set_prolog_flag(multi_arity_warnings,on).
