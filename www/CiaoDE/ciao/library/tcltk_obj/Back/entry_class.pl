%%---------------------------------------------------------------------
%%
%% ENTRY CLASS
%%
%%---------------------------------------------------------------------

:- class(entry_class).
:- use_module(library(lists),[append/3]).
:- inherit_class(library('tcltk_obj/widget_class')).

:- use_module(library('tcltk/examples/tk_test_aux')).
:- use_module(library(tcltk)).

:- data        textvariable/1.
:- inheritable textvariable/1.

textvariable('aux').

:- export(set_textvariable/1).

%%---------------------------------------------------------------------
%D:- pred set_textvariable(+Variable) :: atom
%D	# "Variable, specifies name of the Tcl variable".
%%---------------------------------------------------------------------
set_textvariable(Textvariable) :-
	atom(Textvariable),
	set_fact(textvariable(Textvariable)),
	notify_changes.

:- export([get_textvariablevalue_number/1]).
:- export([get_textvariablevalue_string/1]).

%%---------------------------------------------------------------------
%D:- pred get_textvariablevalue(-Value) :: num
%D	# "Value is the contain of the Tcl variable associated to the entry.".
%%---------------------------------------------------------------------
%get_textvariablevalue(Textvariable,Y) :-
get_textvariablevalue_number(Y) :-
	textvariable(Textvariable_aux),
	atom_concat('$',Textvariable_aux,Textvariable),
	owner(OW),
	OW:interp(I),
	tcl_eval(I,[set,'aux',write(Textvariable)],Z),
	number_codes(Y,Z).	

get_textvariablevalue_string(Y) :-
	textvariable(Textvariable_aux),
	atom_concat('$',Textvariable_aux,Textvariable),
	owner(OW),
	OW:interp(I),
	tcl_eval(I,[set,'aux',write(Textvariable)],Z),
	atom_codes(Y,Z).	

:- export([set_textvariablevalue_string/1]).
:- export([set_textvariablevalue_number/1]).

%%---------------------------------------------------------------------
%D:- pred set_textvariablevalue(+Value) :: num
%D	# "Specifies the Value of the Tcl variable associated to the entry.".
%%---------------------------------------------------------------------
%set_textvariablevalue(Textvariable,Y) :-
set_textvariablevalue_string(Y) :-
%	display('En el textvariable'),nl,
	textvariable(Textvariable),
%	display(Textvariable),display(Y),nl,
	owner(OW),
	OW:interp(I),
	tcl_eval(I,[set,write(Textvariable),' ',write(Y)],_).
%	display('Fin text').

set_textvariablevalue_number(Y) :-
%	display('En el textvariable'),nl,
	textvariable(Textvariable),
	number_codes(Y,Y1),
	atom_codes(Y2,Y1),
%	display(Textvariable),display(Y),nl,
	owner(OW),
	OW:interp(I),
	tcl_eval(I,[set,write(Textvariable),' ',write(Y2)],_).
%	display('Fin text').

:- export([get_textvariable/1]).

%%---------------------------------------------------------------------
%D:- pred get_textvariable(-Variable) :: atom
%D	# "Gets the name of the Tcl variable associated to the entry".
%%---------------------------------------------------------------------
get_textvariable(Textvariable) :-
	textvariable(Textvariable).

:- data        justify/1.
:- inheritable justify/1.

justify('left').

:- export(set_justify/1).

%%---------------------------------------------------------------------
%D:- pred set_justify(+How) :: atom
%D	# "How specifies how to justify the text in the entry. How must be one of the values left, right or center.  This option defaluts to left.".
%%---------------------------------------------------------------------
set_justify(Side) :-
	atom(Side),
	set_fact(justify(Side)),
	notify_changes.

:- export([get_justify/1]).

%%---------------------------------------------------------------------
%D:- pred set_justify(-How) :: atom
%D	# "Gets how is justified the text.".
%%---------------------------------------------------------------------
get_justify(Side) :-
	justify(Side).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1,creation_bind/1]).

%%---------------------------------------------------------------------
%D:- pred tcl_name(-Shape) :: atom 
%D	# "Specifies the name of the Widget. In this case is entry.".
%%---------------------------------------------------------------------
tcl_name(entry).

%%---------------------------------------------------------------------
%D:- pred creation_options(-OptionsList) :: list
%D	# "Creates a list with the options supported by the entry.".
%%---------------------------------------------------------------------
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
