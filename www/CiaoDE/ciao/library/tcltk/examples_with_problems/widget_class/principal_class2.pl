%%---------------------------------------------------------------------
%%
%% TCL WIDGET
%%
%%---------------------------------------------------------------------

:- class(principal_class,[],[objects]).

:- use_class(library('tcltk/examples/widget_class/widget_class')).

:- use_module(library(system)).
:- use_module(library(strings)).
:- use_module(library(lists),[append/3]).
:- use_module(library('tcltk/tcltk')).
:- use_module(library('tcltk/tcltk_low_level')).
:- use_module(library(aggregates)).


:- data item/2.
:- data interp/1.

%%---------------------------------------------------------------------
%% CONSTRUCTOR/DESTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

principal_class :-
	tcl_new(I),
	asserta_fact(interp(I)).
%	self(ID),
%	tcl_eval(I,[canvas, '.canvas'],ID),
%	tcl_eval(I,[pack, '.canvas'],ID1).

principal_class([]) :-
	principal_class.

principal_class([Item|Next]) :-
	( add_item(Item) ; true ),
	!,
	principal_class(Next).

:- set_prolog_flag(multi_arity_warnings,on).

destructor :-
	self(Principal),
%	command(["destroy .canvas",Canvas),
	retract_fact(item(Widget,_)),
	Widget :remove_owner(Principal),
	fail.
destructor :-
	interp(I),
	tcl_delete(I),
	retract_fact(interp(_)).

%%---------------------------------------------------------------------
%% ADD/REMOVE ITEMS
%%---------------------------------------------------------------------

:- export(add_item/1).
:- export(remove_item/1).
:- export(item_changed/1).

add_item(Widget) :-
	\+ item(Widget,_),
	Widget instance_of widget_class,
	assertz_fact(item(Widget,hidden)),
	self(Principal),
	Widget:add_owner(Principal),
	!.
add_item(_).

remove_item(Widget) :-
	hide_item(Widget),
	retract_fact(item(Widget,_)),
	Widget instance_of widget_class,
	self(Principal),
	Widget:remove_owner(Principal),
	!.
remove_item(_).


item_changed(Widget) :-
	hide_item(Widget),
	show_item(Widget).

%%---------------------------------------------------------------------
%% SHOW / HIDE ENTIRE WIDGET
%%---------------------------------------------------------------------

:- export(show/0).

show :-
	item(Widget,hidden),
	show_item(Widget),
	fail.
show.

:- export(hide/0).

hide :-
	item(Widget,shown),
	hide_item(Widget),
	fail.
hide.

%%---------------------------------------------------------------------
%% SHOW / HIDE SPECIFIC ITEMS
%%---------------------------------------------------------------------

:- export(show_item/1).

show_item(Widget) :-
%	self(Pr),
	item(Widget,hidden),
	Widget instance_of widget_class,
	Widget:tcl_name(ItemName), 
	atom_concat('.',ItemName,Name),
%	atom_concat(Name,Pr,Var),
	atom_concat(Name,Widget,Var),
	Widget:creation_options(Opts),
display(Opts),
nl,
	Widget:creation_position(Post),
	Widget:creation_bind(Bind),
	interp(I),
	tcl_eval(I,[ItemName,Var|Opts],_),
	tcl_eval(I,[pack,Var|Post],_),
	tcl_eval(I,[bind,Var|Bind],_),
	retract_fact(item(Widget,hidden)),
	asserta_fact(item(Widget,shown)).

:- export(set_variable/1).

set_variable(X) :-
	display(X),
	interp(I),
%	tcl_eval(I,[set,X,'0'],_).
	tcl_eval(I,[puts,X],_).

:- export(hide_item/1).

hide_item(Widget) :-
%	self(ID),
	retract_fact(item(Widget,shown)),
%	command([".canvas",Canvas," delete tag",Shape]),
%	interp(I),
%	tcl_eval(I,[".canvas"," delete tag",Shape],_),
	asserta_fact(item(Widget,hidden)).

%%-------------------------------------------------------------------
%% TO USE EVENTS
%%-------------------------------------------------------------------

:- export(event_loop/0).

event_loop :-
	interp(I),
	tk_event_loop(I).


%%---------------------------------------------------------------------
%% MACROS
%%---------------------------------------------------------------------
		

codify([],"").

codify([''|Next],[C,C|CNext]) :-
	!,
	atom_codes('\"',[C]),
	codify(Next,CNext).

codify([[]|Next],[C,C|CNext]) :-
	!,
	atom_codes('\"',[C]),
	codify(Next,CNext).


codify([X|Next],Str) :-
	atom(X),
	!,
	atom_codes(X,XCodes),
	codify(Next,CNext),
	append(XCodes,CNext,Str).

codify([X|Next],Str) :-
	number(X),
	number_codes(X,XCodes),
	!,
	codify(Next,CNext),
	append(XCodes,CNext,Str).

codify([X|Next],Str) :-
	!,
	codify(Next,CNext),
	append(X,CNext,Str).
