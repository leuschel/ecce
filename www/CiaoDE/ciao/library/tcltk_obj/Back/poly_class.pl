%%---------------------------------------------------------------------
%%
%% POLYGON CLASS
%%
%%---------------------------------------------------------------------

:- class(poly_class).

:- inherit_class(library('tcltk_obj/shape_class')).

%:- implements(library('class/examples/geometry/mobile')).
:- use_module(library(lists),[append/3]).

%%---------------------------------------------------------------------
%% POINT LIST
%%---------------------------------------------------------------------

:- data        point_list/1.
:- inheritable point_list/1.

validate_points([]).

validate_points([(X,Y)|N]) :-
	number(X),
	number(Y),
	!,
	validate_points(N).

:- export(set_vertices/1).
%%---------------------------------------------------------------------
%D:- pred set_vertices(+ListofPoints) :: list
%D        # "The arguments of the list specify  the  coordinates  for
%D       three  or  more  points that define a closed polygon.  The
%D       first and last points may be the same.
%D       After  the  coordinates  there  may  be  any   number   of
%D       option-value pairs, each of which sets one of the configu-
%D       ration options for  the  item.".
%%---------------------------------------------------------------------

set_vertices(L) :-
	validate_points(L),
	set_fact(point_list(L)),
	notify_changes.

:- export(get_vertices/1).
%%---------------------------------------------------------------------
%D:- pred get_vertices(-ListofPoints) :: list
%D        # "Gets the list of vertices of the polygon.".
%%---------------------------------------------------------------------

get_vertices(L) :-
	point_list(L).

%%---------------------------------------------------------------------
%% COLOR 
%%---------------------------------------------------------------------

:- data         outline/1.
:- inheritable  outline/1.

outline(black).  % default color.

:- export(get_outline/1).

get_outline(Outline) :-
	!,
	outline(Outline).

:- export(set_outline/1).

set_outline(Outline) :-
	atom(Outline),
	retract_fact(outline(_)),
	asserta_fact(outline(Outline)),
	notify_changes.

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

points2options([],[]).

points2options([(X,Y)|Np],[X,Y|No]) :-
	points2options(Np,No).

:- export([tcl_name/1,creation_options/1]).

creation_options(Options) :-
	point_list(Points),
	points2options(Points,Opts),
	inherited creation_options(Other),
	append(Opts,Other,Options).

%%---------------------------------------------------------------------
%D:- pred tcl_name(+Shape) :: atom 
%D        # "Specifies the name of the Shape. In this case is polygon.".
%%---------------------------------------------------------------------

tcl_name(polygon).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

poly_class.

poly_class(PointList) :-
	validate_points(PointList),
	!,
	set_vertices(PointList).

poly_class(Owner) :-
	shape_class(Owner).

poly_class(PointList,Owner) :-
	shape_class(Owner),
	set_vertices(PointList).

:- set_prolog_flag(multi_arity_warnings,on).
