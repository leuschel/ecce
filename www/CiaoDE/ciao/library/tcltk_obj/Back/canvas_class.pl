%%---------------------------------------------------------------------
%%
%% TCL CANVAS WIDGET
%%
%%---------------------------------------------------------------------

:- class(canvas_class,[],[objects]).

:- use_class(library('tcltk_obj/window_class')).
:- use_class(library('tcltk_obj/shape_class')).

:- use_module(library(system)).
:- use_module(library(strings)).
:- use_module(library(lists),[append/3]).
:- use_module(library('tcltk/tcltk')).
:- use_module(library('tcltk/tcltk_low_level')).


:- data shape/2.
:- data interp/1.
:- data        owner/1.
%:- inheritable owner/1.

%%---------------------------------------------------------------------
%% CONSTRUCTOR/DESTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

canvas_class. 


canvas_class([]) :-
	canvas_class.

canvas_class([Shape|Next]) :-
	( add_shape(Shape) ; true ),
	!,
	canvas_class(Next).

:- set_prolog_flag(multi_arity_warnings,on).

destructor :-
	self(Canvas),
	retract_fact(shape(Shape,_)),
	Shape:remove_owner(Canvas),
	fail.

%%---------------------------------------------------------------------
%% ADD/REMOVE ITEMS
%%---------------------------------------------------------------------

:- export(add_shape/1).
:- export(remove_shape/1).
:- export(shape_changed/1).

add_shape(Shape) :-
	\+ shape(Shape,_),
	Shape instance_of shape_class,
	assertz_fact(shape(Shape,hidden)),
	self(Canvas),
	Shape:add_owner(Canvas),
	!.
add_shape(_).

remove_shape(Shape) :-
	hide_shape(Shape),
	retract_fact(shape(Shape,_)),
	Shape instance_of shape_class,
	self(Canvas),
	Shape:remove_owner(Canvas),
	!.
remove_shape(_).


shape_changed(Shape) :-
	hide_shape(Shape),
	show_shape(Shape).

%%---------------------------------------------------------------------
%% SHOW / HIDE ENTIRE CANVAS
%%---------------------------------------------------------------------

:- export(show/0).

show :-
	shape(Shape,hidden),
	show_shape(Shape),
	fail.
show.

:- export(hide/0).

hide :-
	shape(Shape,shown),
	hide_shape(Shape),
	fail.
hide.

%%---------------------------------------------------------------------
%% SHOW / HIDE SPECIFIC ITEMS
%%---------------------------------------------------------------------

:- export(show_shape/1).

show_shape(Shape) :-
	owner(OW),
	self(Canvas),
	shape(Shape,hidden),
	Shape instance_of shape_class,
	Shape:tcl_name(ItemName),
	Shape:creation_options(Opts),
	OW:interp(I),
	X='.',
        append(Opts,[min(tags)|write(Shape)],Opts1),
	tcl_eval(I,[write(X),write(OW),write(X),write(Canvas),' ',' create ',ItemName|Opts1],_),
	retract_fact(shape(Shape,hidden)),
	asserta_fact(shape(Shape,shown)).

:- export(hide_shape/1).

hide_shape(Shape) :-
	self(Canvas),
	retract_fact(shape(Shape,shown)),
	owner(OW),
	OW:interp(I),
	X='.',
	tcl_eval(I,[write(X),write(OW),write(X),write(Canvas),' delete ',write(Shape)],_),
	asserta_fact(shape(Shape,hidden)).


%%---------------------------------------------------------------------
%% MACROS
%%---------------------------------------------------------------------
self_codes(S) :-
	self(Me),
	instance_codes(Me,S).

:- inheritable(self_codes/1).

%%---------------------------------------------------------------------

:- export([add_owner/1,remove_owner/1]).

add_owner(Owner) :-
	\+ owner(Owner),
	Owner instance_of window_class,
	assertz_fact(owner(Owner)),
	self(Menu),
	Owner:add_canvas(Menu),
	!.
add_owner(_).


remove_owner(Owner) :-
	retract_fact(owner(Owner)),
	Owner instance_of window_class,
	self(Menu),
	Owner:remove_canvas(Menu),
	!.

remove_owner(_).

%%
%%
%%
%%---------------------------------------------------------------------
%% WIDTH
%%---------------------------------------------------------------------

:- data        width/1.
:- inheritable width/1.

width(100).

:- export(set_width/1).

set_width(Width) :-
	num(Width),
	set_fact(width(Width)),
	notify_changes.

:- export(get_width/1).

get_width(Width) :-
	width(Width).

%%---------------------------------------------------------------------
%% HEIGHT
%%---------------------------------------------------------------------

:- data        height/1.
:- inheritable height/1.

height(50).

:- export(set_height/1).

set_height(Height) :-
	num(Height),
	set_fact(height(Height)),
	notify_changes.

:- export(get_height/1).

get_height(Height) :-
	height(Height).

%%---------------------------------------------------------------------
%% SIDE
%%---------------------------------------------------------------------

:- data        side/1.
:- inheritable side/1.

side('top').

:- export(set_side/1).

set_side(Side) :-
	atom(Side),
	set_fact(side(Side)),
	notify_changes.

:- export(get_side/1).

get_side(Side) :-
	side(Side).

%%---------------------------------------------------------------------
%% EXPAND
%%---------------------------------------------------------------------

:- data        expand/1.
:- inheritable expand/1.

expand('0').

:- export(set_expand/1).

set_expand(Expand) :-
	atom(Expand),
	set_fact(expand(Expand)),
	notify_changes.

:- export(get_expand/1).

get_expand(Expand) :-
	expand(Expand).

%%---------------------------------------------------------------------
%% FILL
%%---------------------------------------------------------------------

:- data        fill/1.
:- inheritable fill/1.

fill('none').

:- export(set_fill/1).

set_fill(Fill) :-
	atom(Fill),
	set_fact(fill(Fill)),
	notify_changes.

:- export(get_fill/1).

get_fill(Fill) :-
	fill(Fill).

%%---------------------------------------------------------------------
%% PADX
%%---------------------------------------------------------------------

:- data        padx/1.
:- inheritable padx/1.

padx('2').

:- export(set_padx/1).

set_padx(Padx) :-
	atom(Padx),
	set_fact(padx(Padx)),
	notify_changes.

:- export(get_padx/1).

get_padx(Padx) :-
	padx(Padx).

%%---------------------------------------------------------------------
%% PADY
%%---------------------------------------------------------------------

:- data        pady/1.
:- inheritable pady/1.

pady('2').

:- export(set_pady/1).

set_pady(Pady) :-
	atom(Pady),
	set_fact(pady(Pady)),
	notify_changes.

:- export(get_pady/1).

get_pady(Pady) :-
	pady(Pady).


%%
%%
%%
:- export([tcl_name/1,creation_options/1,creation_position/1]).

tcl_name(_) :- fail.

creation_options([min(width),W,min(height),H]) :-
%	background(B),
%	borderwidth(O),
	width(W),
	height(H).
%	highlightbackground(HB),
%	highlightcolor(HC),
%	relief(R).

creation_position([' ',min(side),S,min(expand),E,min(fill),F,min(padx),X,min(pady),Y]) :-
	side(S),
	expand(E),
	fill(F),
	padx(X),
	pady(Y).

%creation_bind([' ',Eventtype,br(C)]) :-
%	event_type(Eventtype),
%	action(C).

:- inheritable(notify_changes/0).

notify_changes:-
	self(Canvas),
	owner(AnOwner),
	AnOwner instance_of window_class,
	AnOwner:canvas_changed(Canvas),
	fail.
notify_changes.
