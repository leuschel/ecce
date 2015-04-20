%%---------------------------------------------------------------------
%%
%% POLYGON CLASS
%%
%%---------------------------------------------------------------------

:- class(poly_class).

:- inherit_class(library('tcltk/examples_with_problems/class/shape_class')).

:- implements(library('class/examples/geometry/mobile')).
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

set_vertices(L) :-
        validate_points(L),
        set_fact(point_list(L)),
        notify_changes.

:- export(get_vertices/1).

get_vertices(L) :-
        point_list(L).

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
