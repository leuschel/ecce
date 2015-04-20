%%---------------------------------------------------------------------
%%
%% LINE CLASS
%%
%%---------------------------------------------------------------------

:- module(line_class_doc,[],[objects,assertions,isomodes,regtypes]).

%:- inherit_class(library('tcltk/examples/interface/shape_class')).

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
       two or more points that describe a serie of connected line segments.".
%%---------------------------------------------------------------------
set_vertices(L) :-
        validate_points(L),
        set_fact(point_list(L)),
        notify_changes.

:- export(get_vertices/1).

%%---------------------------------------------------------------------
:- pred get_vertices(-ListofVertices) :: list
        # "Gets the list of points of the line.".
%%---------------------------------------------------------------------
get_vertices(L) :-
        point_list(L).

%%---------------------------------------------------------------------
%% ARROW 
%%---------------------------------------------------------------------


arrow(none).  % default color.

:- export(set_arrow/1).

%%---------------------------------------------------------------------
:- pred set_arrow(+Where) :: atom
        # "@var{Where} indicates whether or not arrowheads are to be drawn at one or both ends of the line. @var{Where} must have one of the next values: none ( for no arrowheads ), first (for an arrowhead at the first point of the line), last (for an arrowhead at the last point of the line), or both (for arrowheads at both ends). This option defaults to none.".
%%---------------------------------------------------------------------
set_arrow(Arrow) :-
        atom(Arrow),
        retract_fact(arrow(_)),
        asserta_fact(arrow(Arrow)),
        notify_changes.

:- export(get_arrow/1).

%%---------------------------------------------------------------------
:- pred get_arrow(-Where) :: atom
        # "Gets position of the arrowheads.".
%%---------------------------------------------------------------------
get_arrow(Arrow) :-
        !,
        arrow(Arrow).

%%---------------------------------------------------------------------
%% ARROW SHAPE 
%%---------------------------------------------------------------------


arrowshape(none).  % default color.

:- export(set_arrowshape/1).

:- comment(hide,'set_arrowshape'/1).

set_arrowshape(Arrowshape) :-
        atom(Arrowshape),
        retract_fact(arrowshape(_)),
        asserta_fact(arrowshape(Arrowshape)),
        notify_changes.

:- export(get_arrowshape/1).

:- comment(hide,'get_arrowshape'/1).
get_arrowshape(Arrowshape) :-
        !,
        arrowshape(Arrowshape).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

points2options([],[]).

points2options([(X,Y)|Np],[X,Y|No]) :-
        points2options(Np,No).

:- export([tcl_name/1,creation_options/1]).

%%---------------------------------------------------------------------
:- pred creation_options(-OptionsList) :: list
        # "Creates a list with the options supported by the line.".
%%---------------------------------------------------------------------
creation_options(Options) :-
        point_list(Points),
        points2options(Points,Opts),
        arrow(A),
        append([min(arrow)],[A],Opts1),
        append(Opts,Opts1,Opts3),
        creation_options(Other),
        append(Opts3,Other,Options).


%%---------------------------------------------------------------------
:- pred tcl_name(+Shape) :: atom 
        # "Specifies the name of the @var{Shape}. In this case is line.".
%%---------------------------------------------------------------------
tcl_name(line).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

line_class.

line_class(PointList) :-
        validate_points(PointList),
        !,
        set_vertices(PointList).

line_class(Owner) :-
        shape_class(Owner).

line_class(PointList,Owner) :-
        shape_class(Owner),
        set_vertices(PointList).

:- set_prolog_flag(multi_arity_warnings,on).
