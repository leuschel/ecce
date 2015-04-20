%%---------------------------------------------------------------------
%%
%% ENTRY CLASS
%%
%%---------------------------------------------------------------------

:- module(entry_class_doc,[],[objects,assertions,isomodes,regtypes]).

:- use_module(library(lists),[append/3]).
%:- inherit_class(library('tcltk/examples/interface/widget_class')).

:- use_module(library('tcltk/examples/tk_test_aux')).
:- use_module(library(tcltk)).


textvariable('aux').

:- export(set_textvariable/1).

%%---------------------------------------------------------------------
:- pred set_textvariable(+Variable) :: atom
        # "@var{Variable} specifies the name of the Tcl variable for the entry.".
%%---------------------------------------------------------------------
set_textvariable(Textvariable) :-
        atom(Textvariable),
        set_fact(textvariable(Textvariable)),
        notify_changes.

:- export([get_textvariable/1]).

%%---------------------------------------------------------------------
:- pred get_textvariable(-Variable) :: atom
        # "Gets the name of the Tcl @var{Variable} associated to the entry".
%%---------------------------------------------------------------------
get_textvariable(Textvariable) :-
        textvariable(Textvariable).

:- export([set_textvariablevalue/1]).

%%---------------------------------------------------------------------
:- pred set_textvariablevalue(+Value) :: num
        # "Specifies the @var{Value} of the Tcl variable associated to the entry.".
%%---------------------------------------------------------------------
%set_textvariablevalue(Textvariable,Y) :-
set_textvariablevalue(Y) :-
        textvariable(Textvariable),
        owner(OW),
        OW:interp(I),
        tcl_eval(I,[set,write(Textvariable),' ',write(Y)],_).

:- export([get_textvariablevalue/1]).

%%---------------------------------------------------------------------
:- pred get_textvariablevalue(-Value) :: num
        # "@var{Value} is the value of the Tcl variable associated to the entry.".
%%---------------------------------------------------------------------
%get_textvariablevalue(Textvariable,Y) :-
get_textvariablevalue(Y) :-
        textvariable(Textvariable_aux),
        atom_concat('$',Textvariable_aux,Textvariable),
        owner(OW),
        OW:interp(I),
        tcl_eval(I,[set,'aux',write(Textvariable)],Z),
        number_codes(Y,Z).



justify('left').

:- export(set_justify/1).

%%---------------------------------------------------------------------
:- pred set_justify(+How) :: atom
        # "@var{How} specifies how to justify the text in the entry. @var{How} must be one of the values left, right or center.  This option defaluts to left.".
%%---------------------------------------------------------------------
set_justify(Side) :-
        atom(Side),
        set_fact(justify(Side)),
        notify_changes.

:- export([get_justify/1]).

%%---------------------------------------------------------------------
:- pred get_justify(-How) :: atom
        # "Gets @var{How} is justified the text.".
%%---------------------------------------------------------------------
get_justify(Side) :-
        justify(Side).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

%:- export([tcl_name/1,creation_options/1,creation_bind/1]).
:- export([tcl_name/1,creation_options/1]).

:- comment(hide/tcl_name/1).

%%---------------------------------------------------------------------
:- pred tcl_name(-Widget) :: atom 
        # "Specifies the name of the @var{Widget}. In this case is entry.".
%%---------------------------------------------------------------------
tcl_name(entry).

%%---------------------------------------------------------------------
:- pred creation_options(-OptionsList) :: list
        # "Creates a list with the options supported by the entry.".
%%---------------------------------------------------------------------
%creation_options([' ',min(justify),S,min(textvariable),T,''|Other]) :-
creation_options([' ',min(justify),S,''|Other1]) :-
        justify(S),
        textvariable(T),
        creation_options(Other),
        append(Other,[min(textvariable)|write(T)],Other1).

%creation_bind([' ','<Any-Key>',br([prolog_one_event,dq(write(execute(widget3:anadir(inputval(7)))))])]) .


%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------
:- comment(hide,'class$call'/3).
:- comment(hide,'class$used'/2).

:- set_prolog_flag(multi_arity_warnings,off).

entry_class.
entry_class(Owner) :-
        entry_class(Owner).


:- set_prolog_flag(multi_arity_warnings,on).
