%%---------------------------------------------------------------------
%%
%% POLYGON CLASS
%%
%%---------------------------------------------------------------------

:- module(poly_class_doc,[],[objects,assertions,isomodes,regtypes]).

%:- inherit_class(library('tcltk/examples/interface/shape_class')).

%:- implements(library('class/examples/geometry/mobile')).
:- use_module(library(lists),[append/3]).

:- comment(hide,'class$call'/3).
:- comment(hide,'class$used'/2).
%%---------------------------------------------------------------------
%% POINT LIST
%%---------------------------------------------------------------------

validate_points([]).

validate_points([(X,Y)|N]) :-
        number(X),
        number(Y),
        !,
        validate_points(N).

:- export(set_vertices/1).

%%---------------------------------------------------------------------
:- pred set_vertices(+ListofVertices) :: list
        # "The arguments of the list specify  the  coordinates  for
       three  or  more  points that define a closed polygon.  The
       first and last points may be the same.
       After  the  coordinates  there  may  be  any   number   of
       option-value pairs, each of which sets one of the configu-
       ration options for  the  item.".
%%---------------------------------------------------------------------
set_vertices(L) :-
        validate_points(L),
        set_fact(point_list(L)),
        notify_changes.

:- export(get_vertices/1).

%%---------------------------------------------------------------------
:- pred get_vertices(-ListofVertices) :: list
        # "Gets the list of vertices of the polygon.".
%%---------------------------------------------------------------------
get_vertices(L) :-
        point_list(L).

%%---------------------------------------------------------------------
%% COLOR 
%%---------------------------------------------------------------------


outline(black).  % default color.

:- export(set_outline/1).

%%---------------------------------------------------------------------
:- pred set_outline(+Color) :: atom
        # "@var{Color} specifies the color to be used for drawing the polygon's outline. This option defaults to black.".
%%---------------------------------------------------------------------
set_outline(Outline) :-
        atom(Outline),
        retract_fact(outline(_)),
        asserta_fact(outline(Outline)),
        notify_changes.

:- export(get_outline/1).

%%---------------------------------------------------------------------
:- pred get_outline(-Color) :: atom
        # "Gets poly's outline @var{Color}.".
%%---------------------------------------------------------------------
get_outline(Outline) :-
        !,
        outline(Outline).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

points2options([],[]).

points2options([(X,Y)|Np],[X,Y|No]) :-
        points2options(Np,No).

:- export([tcl_name/1,creation_options/1]).

%%---------------------------------------------------------------------
:- pred creation_options(-OptionsList) :: list
        # "Creates a list with the options supported by the polygon.".
%%---------------------------------------------------------------------
creation_options(Options) :-
        point_list(Points),
        points2options(Points,Opts),
        creation_options(Other),
        append(Opts,Other,Options).


:- comment(hide,tcl_name/1).
%%---------------------------------------------------------------------
:- pred tcl_name(+Shape) :: atom 
        # "Specifies the name of the @var{Shape}. In this case is polygon.".
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
