%%---------------------------------------------------------------------
%%
%% OVAL CLASS
%%
%%---------------------------------------------------------------------

:- class(oval_class).

:- inherit_class(library('tcltk_obj/shape_class')).

%%---------------------------------------------------------------------
%% COORDINATES
%%---------------------------------------------------------------------

:- data        coord/4.
:- inheritable coord/4.

coord(0,0,0,0).
%%---------------------------------------------------------------------
%:- pred coord(+x1,+y1,+x2,+y2) :: int * int * int * int
%       # "The arguments x1, y1, x2, and y2 give the  coordinates  of
%      two  diagonally  opposite  corners of a rectangular region
%       enclosing the oval.".
%%---------------------------------------------------------------------

:- export([set_width/1,set_height/1,set_center/2]).

%%---------------------------------------------------------------------
%:- pred set_width(+Width) :: int
%        # "Specifies shape's width.".
%%---------------------------------------------------------------------
set_width(W) :-
	number(W),
	W >= 0,
	retract_fact(coord(X,Y,_,H)),
	asserta_fact(coord(X,Y,W,H)),
	notify_changes.

%%---------------------------------------------------------------------
%:- pred set_height(+Height) :: int
%        # "Specifies shape's heigth.".
%%---------------------------------------------------------------------
set_height(H) :-
	number(H),
	H >= 0,
	retract_fact(coord(X,Y,W,_)),
	asserta_fact(coord(X,Y,W,H)),
	notify_changes.

%%---------------------------------------------------------------------
%:- pred set_center(+X,+Y) :: int * int
%        # "Specifies shape's center.".
%%---------------------------------------------------------------------
set_center(X,Y) :-
	number(X),
	number(Y),
	retract_fact(coord(_,_,W,H)),
	asserta_fact(coord(X,Y,W,H)),
	notify_changes.

:- export([get_width/1,get_height/1,get_center/2]).
%%---------------------------------------------------------------------
%:- pred get_width(-Width) :: int
%        # "Gets shape's width.".
%%---------------------------------------------------------------------
get_width(W) :-
	coord(_,_,W,_).
%%---------------------------------------------------------------------
%:- pred get_height(-Width) :: int
%        # "Gets shape's height.".
%%---------------------------------------------------------------------
get_height(H) :-
	coord(_,_,_,H).
%%---------------------------------------------------------------------
%:- pred get_center(-X,-Y) :: int * int
%        # "Gets shape's center.".
%%---------------------------------------------------------------------
get_center(X,Y) :-
	coord(X,Y,_,_).

%%---------------------------------------------------------------------
%% COLOR 
%%---------------------------------------------------------------------

:- data         outline/1.
:- inheritable  outline/1.

outline(black).  % default color.

:- export(get_outline/1).
%%---------------------------------------------------------------------
%:- pred get_outline(-X) :: atom
%        # "Gets shape's outline color.".
%%---------------------------------------------------------------------

get_outline(Outline) :-
	!,
	outline(Outline).

:- export(set_outline/1).
%%---------------------------------------------------------------------
%:- pred set_outline(+X) :: atom
%        # "Sets shape's outline color.".
%%---------------------------------------------------------------------
set_outline(Outline) :-
	atom(Outline),
	retract_fact(outline(_)),
	asserta_fact(outline(Outline)),
	notify_changes.

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).
%%---------------------------------------------------------------------
%:- pred tcl_name(+Shape) :: atom 
%        # "Specifies the name of the Shape. In this case is oval.".
%%---------------------------------------------------------------------
tcl_name(oval).

creation_options([X1,Y1,X2,Y2,min(outline),O|Other]) :-
	coord(X,Y,W,H),
	W2 is W / 2,
	H2 is H / 2,
	X1 is X-W2,
	X2 is X+W2,
	Y1 is Y-H2,
	Y2 is Y+H2,
	outline(O),
	inherited creation_options(Other).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

oval_class.
oval_class(Owner) :-
	shape_class(Owner).

oval_class((X,Y),W,H,Owner) :-
	shape_class(Owner),
	set_width(W),
	set_height(H),
	set_center(X,Y).

oval_class((X,Y),W,H) :-
	set_width(W),
	set_height(H),
	set_center(X,Y).

:- set_prolog_flag(multi_arity_warnings,on).
