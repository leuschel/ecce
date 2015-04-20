%%---------------------------------------------------------------------
%%
%% TEXT CLASS
%%
%%---------------------------------------------------------------------

:- class(text_class).

:- inherit_class(library('tcltk_obj/shape_class')).
%:- implements(library('class/examples/geometry/mobile')).
%:- implements(library('class/examples/class/mobile')).

%%---------------------------------------------------------------------
%% COORDINATES
%%---------------------------------------------------------------------

:- data        coord/2.
:- inheritable coord/2.

coord(0,0).

:- export([set_point/2]).

set_point(X,Y) :-
 	number(X),
 	number(Y),
	retract_fact(coord(_,_)),
	asserta_fact(coord(X,Y)),
	notify_changes.
%%---------------------------------------------------------------------
%%  TEXT 
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
%%  ANCHOR 
%%---------------------------------------------------------------------

:- data        anchor/1.
:- inheritable anchor/1.

anchor('center').

:- export(set_anchor/1).

set_anchor(Anchor) :-
	atom(Anchor),
	set_fact(anchor(Anchor)),
	notify_changes.

:- export(get_anchor/1).

get_anchor(Anchor) :-
	anchor(Anchor).

%%---------------------------------------------------------------------
%%  FONT 
%%---------------------------------------------------------------------

:- data        font/1.
:- inheritable font/1.

font('arial').

:- export(set_font/1).

set_font(Font) :-
	atom(Font),
	set_fact(font(Font)),
	notify_changes.

:- export(get_font/1).

get_font(Font) :-
	font(Font).

%%---------------------------------------------------------------------
%%  JUSTIFY 
%%---------------------------------------------------------------------

:- data        justify/1.
:- inheritable justify/1.

justify('center').

:- export(set_justify/1).

set_justify(Justify) :-
	atom(Justify),
	set_fact(justify(Justify)),
	notify_changes.

:- export(get_justify/1).

get_justify(Justify) :-
	justify(Justify).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

tcl_name(text).

creation_options([X,Y,min(text),T,min(anchor),A,min(font),F,min(justify),J|Other]) :-
	coord(X,Y),
	text(T),
	anchor(A),
	font(F),
	justify(J),
	inherited creation_options(Other).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

text_class.
text_class(Owner) :-
	shape_class(Owner).

text_class(X,Y,Owner) :-
	shape_class(Owner),
	set_point(X,Y).

text_class(X,Y) :-
	set_point(X,Y).

:- set_prolog_flag(multi_arity_warnings,on).
