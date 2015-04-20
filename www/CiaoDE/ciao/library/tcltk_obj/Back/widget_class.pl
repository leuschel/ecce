%%---------------------------------------------------------------------
%%
%% WIDGET CLASS
%%
%%---------------------------------------------------------------------

:- class(widget_class,[],[objects]).

:- use_class(library('tcltk_obj/window_class')).


%%---------------------------------------------------------------------
%% ROW
%%---------------------------------------------------------------------

:- data        row/1.
:- inheritable row/1.

row('0').

:- export(set_row/1).

set_row(Row) :-
	atom(Row),
	set_fact(row(Row)),
	notify_changes.

:- export(get_row/1).
 
get_row(Row) :-
	row(Row).

:- data        rowspan/1.
:- inheritable rowspan/1.

rowspan(1).

:- export(set_rowspan/1).

set_rowspan(Rowspan) :-
	num(Rowspan),
	set_fact(rowspan(Rowspan)),
	notify_changes.

:- export(get_rowspan/1).
 
get_rowspan(Rowspan) :-
	rowspan(Rowspan).

%%---------------------------------------------------------------------
%% COLUMN
%%---------------------------------------------------------------------

:- data        column/1.
:- inheritable column/1.

column('0').

:- export(set_column/1).

set_column(Column) :-
	atom(Column),
	set_fact(column(Column)),
	notify_changes.

:- export(get_column/1).

get_column(Column) :-
	column(Column).

:- data        columnspan/1.
:- inheritable columnspan/1.

columnspan(1).

:- export(set_columnspan/1).

set_columnspan(Columnspan) :-
	num(Columnspan),
	set_fact(columnspan(Columnspan)),
	notify_changes.

:- export(get_columnspan/1).

get_columnspan(Columnspan) :-
	columnspan(Columnspan).

%%---------------------------------------------------------------------
%% TEXT
%%---------------------------------------------------------------------

:- data        text/1.
:- inheritable text/1.

text('').

:- export(set_text/1).

set_text(Text) :-
	atom(Text),
	set_fact(text(Text)),
	notify_changes.

:- export(get_text/1).

get_text(Text) :-
	text(Text).

%%---------------------------------------------------------------------
%% FONT
%%---------------------------------------------------------------------

:- data        font/1.
:- inheritable font/1.

font('normal').

:- export(set_font/1).

set_font(Font) :-
	atom(Font),
	set_fact(font(Font)),
	notify_changes.

:- export(get_font/1).

get_font(Font) :-
	font(Font).

%%---------------------------------------------------------------------
%% BACKGROUND
%%---------------------------------------------------------------------

:- data        background/1.
:- inheritable background/1.

background('gray').

:- export(set_background/1).

set_background(Bg) :-
	atom(Bg),
	set_fact(background(Bg)),
	notify_changes.

:- export(get_background/1).

get_background(Bg) :-
	background(Bg).

%%---------------------------------------------------------------------
%% BORDERWIDTH
%%---------------------------------------------------------------------

:- data        borderwidth/1.
:- inheritable borderwidth/1.

borderwidth('2').

:- export(set_borderwidth/1).

set_borderwidth(Bw) :-
	atom(Bw),
	set_fact(borderwidth(Bw)),
	notify_changes.

:- export(get_borderwidth/1).

get_borderwidth(Bw) :-
	borderwidth(Bw).

%%---------------------------------------------------------------------
%% FOREGROUND
%%---------------------------------------------------------------------

:- data        foreground/1.
:- inheritable foreground/1.

foreground('black').

:- export(set_foreground/1).

set_foreground(Fg) :-
	atom(Fg),
	set_fact(foreground(Fg)),
	notify_changes.

:- export(get_foreground/1).

get_foreground(Fg) :-
	foreground(Fg).

%%---------------------------------------------------------------------
%% HIGHLIGHTBACKGROUND
%%---------------------------------------------------------------------

:- data        highlightbackground/1.
:- inheritable highlightbackground/1.

highlightbackground('white').

:- export(set_highlightbackground/1).

set_highlightbackground(Highlightb) :-
	atom(Highlightb),
	set_fact(highlightbackground(Highlightb)),
	notify_changes.

:- export(get_highlightbackground/1).

get_highlightbackground(Highlightb) :-
	highlightbackground(Highlightb).

%%---------------------------------------------------------------------
%% HIGHLIGHTBACKGROUND
%%---------------------------------------------------------------------

:- data        highlightcolor/1.
:- inheritable highlightcolor/1.

highlightcolor('white').

:- export(set_highlightcolor/1).

set_highlightcolor(Highlightb) :-
	atom(Highlightb),
	set_fact(highlightcolor(Highlightb)),
	notify_changes.

:- export(get_highlightcolor/1).

get_highlightcolor(Highlightb) :-
	highlightcolor(Highlightb).

%%---------------------------------------------------------------------
%% WIDTH
%%---------------------------------------------------------------------

:- data        width/1.
:- inheritable width/1.

width('0').

:- export(set_width/1).

set_width(Width) :-
	atom(Width),
	set_fact(width(Width)),
	notify_changes.

:- export(get_width/1).

get_width(Width) :-
	width(Width).

%%---------------------------------------------------------------------
%% RELIEF
%%---------------------------------------------------------------------

:- data        relief/1.
:- inheritable relief/1.

relief('sunken').

:- export(set_relief/1).

set_relief(Relief) :-
	atom(Relief),
	set_fact(relief(Relief)),
	notify_changes.

:- export(get_relief/1).

get_relief(Relief) :-
	relief(Relief).

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

%%---------------------------------------------------------------------
%% EVENT TYPE
%%---------------------------------------------------------------------

:- data        event_type/1.
:- inheritable event_type/1.

event_type('').

:- export(set_event_type/1).

set_event_type(EventT) :-
	atom(EventT),
	set_fact(event_type(EventT)),
	notify_changes.

:- export(get_event_type/1).

get_event_type(EventT) :-
	event_type(EventT).

%%---------------------------------------------------------------------
%% ACTION
%%---------------------------------------------------------------------

:- data        action/1.
:- inheritable action/1.

action('').

:- export(set_action/3).
:- export(set_action/1).

set_action(Predicate) :-
	atom(Predicate),
	set_fact(action([prolog1,dq(write(execute(Predicate)))])),
	notify_changes.

set_action(Input,Output,Predicate) :-
	atom(Predicate),
	atom_concat('$',Input,Input1),
	atom_concat('$prolog_variables(',Output,Output1),
	atom_concat(Output1,')',Output2),
	set_fact(action([set,Input,Input1,'\n',
	                    prolog_one_event,dq(write(execute(Predicate))),'\n',
                            set,Output,Output2])),
%	set_fact(predicate([prolog_one_event,dq(write(execute(Predicate)))])),
	notify_changes.

:- export(get_action/1).

get_action(Predicate) :-
	action(Predicate).

%%---------------------------------------------------------------------
%% VARIABLES
%%---------------------------------------------------------------------

:- data        variables/1.
:- inheritable variables/1.

variables('').

:- export(set_variables/1).

set_variables(Variable) :-
	atom(Variable),
	atom_concat('$',Variable,Variable1),
	set_fact(variables([set,Variable,Variable1])),
	notify_changes.


%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------


:- export([tcl_name/1,creation_options/1,creation_position/1,creation_position_grid/1,creation_bind/1]).

tcl_name(_) :- fail.

creation_options([min(text),dq(T),min(font),dq(F),min(background),B,min(borderwidth),O,min(foreground),G,min(width),W,min(highlightbackground),HB,min(highlightcolor),HC,min(relief),R]) :-
	%self(ID), 
	text(T),
	font(F),
	background(B),
	borderwidth(O),
	foreground(G),
	width(W),
	highlightbackground(HB),
	highlightcolor(HC),
	relief(R).


creation_position([' ',min(side),S,min(expand),E,min(fill),F,min(padx),X,min(pady),Y]) :-
	side(S),
	expand(E),
	fill(F),
	padx(X),
	pady(Y).

creation_position_grid([' ',min(row),R,min(rowspan),S,min(column),C,min(columnspan),P]) :-
	row(R),
	rowspan(S),
	column(C),
	columnspan(P).


creation_bind([' ',Eventtype,br(C)]) :-
	event_type(Eventtype),
	action(C).

:- inheritable(notify_changes/0).

notify_changes:-
	self(Widget),
	owner(AnOwner),
	AnOwner instance_of window_class,
	AnOwner:item_changed(Widget),
	fail.
notify_changes.

:- export([add_owner/1,remove_owner/1]).

add_owner(Owner) :-
	\+ owner(Owner),
	Owner instance_of window_class,
	assertz_fact(owner(Owner)),
	self(Widget),
	Owner:add_item(Widget),
	!.
add_owner(_).


remove_owner(Owner) :-
	retract_fact(owner(Owner)),
	Owner instance_of window_class,
	self(Widget),
	Owner:remove_item(Widget),
	!.

remove_owner(_).

%%---------------------------------------------------------------------
%% CONSTRUCTOR/DESTRUCTOR
%%---------------------------------------------------------------------

:- data        owner/1.
:- inheritable owner/1.


:- set_prolog_flag(multi_arity_warnings,off).

widget_class.  % Not owned

widget_class([]) :- !.

widget_class([AnOwner|Next]) :-
	add_owner(AnOwner),
	!,
	widget_class(Next).

widget_class(AnOwner) :-
	!,
	add_owner(AnOwner).

:- set_prolog_flag(multi_arity_warnings,on).

destructor :-
	self(Widget),
	retract_fact(owner(AnOwner)),
	AnOwner instance_of window_class,     % Owner is still alive
	AnOwner:remove_item(Widget),
	fail.
