%%---------------------------------------------------------------------
%%
%% LABEL CLASS
%%
%%---------------------------------------------------------------------

:- class(label_class).
:- inherit_class(library('tcltk/examples_with_problems/widget_class/widget_class')).

:- data        textvariable/1.
:- inheritable textvariable/1.

textvariable('').

:- export(set_textvariable/1).

set_textvariable(Textvariable) :-
        atom(Textvariable),
        set_fact(textvariable(Textvariable)),
        notify_changes.

:- export([get_textvariable/1]).

get_textvariable(Textvariable) :-
        textvariable(Textvariable).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

tcl_name(label).

creation_options([''|Other]) :-
%       textvariable(T),
        inherited creation_options(Other).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

label_class.
label_class(Owner) :-
        label_class(Owner).


:- set_prolog_flag(multi_arity_warnings,on).
