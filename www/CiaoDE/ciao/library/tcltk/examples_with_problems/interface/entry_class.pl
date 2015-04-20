%%---------------------------------------------------------------------
%%
%% ENTRY CLASS
%%
%%---------------------------------------------------------------------

:- class(entry_class).
:- use_module(library(lists),[append/3]).
:- inherit_class(library('tcltk/examples_with_problems/interface/widget_class')).

%:- use_module(library('tcltk/examples/tk_test_aux')).
:- use_module(library(tcltk)).

:- data        textvariable/1.
:- inheritable textvariable/1.

textvariable('aux').

:- export(set_textvariable/1).

set_textvariable(Textvariable) :-
        atom(Textvariable),
        set_fact(textvariable(Textvariable)),
        notify_changes.

:- export([get_textvariablevalue/1]).

%get_textvariablevalue(Textvariable,Y) :-
get_textvariablevalue(Y) :-
        textvariable(Textvariable_aux),
        atom_concat('$',Textvariable_aux,Textvariable),
        owner(OW),
        OW:interp(I),
        tcl_eval(I,[set,'aux',write(Textvariable)],Z),
        number_codes(Y,Z).

:- export([set_textvariablevalue/1]).

%set_textvariablevalue(Textvariable,Y) :-
set_textvariablevalue(Y) :-
        textvariable(Textvariable),
        owner(OW),
        OW:interp(I),
        tcl_eval(I,[set,write(Textvariable),' ',write(Y)],_).

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
