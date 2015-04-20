%%---------------------------------------------------------------------
%%
%% ARC CLASS
%%
%%---------------------------------------------------------------------

:- module(arc_class_doc,[],[objects,assertions,isomodes,regtypes]).

%:- inherit_class(library('tcltk/examples/class/shape_class')).
:- implements(library('class/examples/geometry/mobile')).

%%---------------------------------------------------------------------
%% COORDINATES
%%---------------------------------------------------------------------

%:- data        coord/4.
%:- inheritable coord/4.

%%---------------------------------------------------------------------
:- export(coord/4).

:- pred coord(+x1,+y1,+x2,+y2) :: int * int * int *int
        # "The  arguments  x1, y1, x2, and y2 give the coordinates of
       two diagonally opposite corners of  a  rectangular  region
       enclosing  the oval that defines the arc.".
%%---------------------------------------------------------------------

coord(0,0,0,0).

:- export([set_width/1,set_height/1,set_center/2]).

%%---------------------------------------------------------------------
:- pred set_width(+Width) :: int
        # "Specifies shape's width.".
%%---------------------------------------------------------------------
set_width(W) :-
        number(W),
        W >= 0,
        retract_fact(coord(X,Y,_,H)),
        asserta_fact(coord(X,Y,W,H)),
        notify_changes.
%%---------------------------------------------------------------------
:- pred set_height(+Height) :: int
        # "Specifies shape's heigth.".
%%---------------------------------------------------------------------

set_height(H) :-
        number(H),
        H >= 0,
        retract_fact(coord(X,Y,W,_)),
        asserta_fact(coord(X,Y,W,H)),
        notify_changes.
%%---------------------------------------------------------------------
:- pred set_center(+X,+Y) :: int * int
        # "Specifies shape's center.".
%%---------------------------------------------------------------------

set_center(X,Y) :-
        number(X),
        number(Y),
        retract_fact(coord(_,_,W,H)),
        asserta_fact(coord(X,Y,W,H)),
        notify_changes.


:- export([get_width/1,get_height/1,get_center/2]).
%%---------------------------------------------------------------------
:- pred get_width(-Width) :: int
        # "Gets shape's width.".
%%---------------------------------------------------------------------

get_width(W) :-
        coord(_,_,W,_).

%%---------------------------------------------------------------------
:- pred get_height(-Width) :: int
        # "Gets shape's height.".
%%---------------------------------------------------------------------
get_height(H) :-
        coord(_,_,_,H).
%%---------------------------------------------------------------------
:- pred get_center(-X,-Y) :: int * int
        # "Gets shape's center.".
%%---------------------------------------------------------------------

get_center(X,Y) :-
        coord(X,Y,_,_).

:- data        angle/1.
:- inheritable angle/1.

angle(0).

:- export(set_angle_start/1).
%%---------------------------------------------------------------------
:- pred set_angle_start(+Angle) :: int
        # "Specifies  the beginning of the angular range occupied by the arc.  
           Degrees  is  given  in  units  of
           degrees   measured   counter-clockwise   from   the
           3-o'clock position;  it may be either  positive  or
           negative.".
%%---------------------------------------------------------------------

set_angle_start(Angle) :-
        number(Angle),
        Angle > 0,
        set_fact(angle(Angle)),
        notify_changes.

:- export([get_angle_start/1]).
%%---------------------------------------------------------------------
:- pred get_angle_start(-Angle) :: int 
        # "Gets the value of the angle.".
%%---------------------------------------------------------------------

get_angle_start(Angle) :-
        angle(Angle).

:- data        style/1.
:- inheritable style/1.

style('pieslice').

:- export(set_style/1).
%%---------------------------------------------------------------------
:- pred set_style(+Angle) :: atom 
        # "Specifies how to draw the arc. If type is pieslice
           (the default) then the arc's region is defined by a
           section  of the oval's perimeter plus two line segments, 
           one between the center of the oval and  each
           end  of  the  perimeter  section.  If type is chord
           then the arc's region is defined by  a  section  of
           the  oval's  perimeter  plus  a single line segment
           connecting the two end points of the perimeter section.
           If  type  is arc then the arc's region consists of a section 
           of the perimeter alone.  In this
           last case the -fill option is ignored.".
%%---------------------------------------------------------------------

set_style(Style) :-
        atom(Style),
        set_fact(style(Style)),
        notify_changes.

:- export([get_style/1]).
%%---------------------------------------------------------------------
:- pred get_style(-Style) :: atom 
        # "Gets the style of the arc.".
%%---------------------------------------------------------------------

get_style(Style) :-
        style(Style).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).
%%---------------------------------------------------------------------
:- pred tcl_name(+Shape) :: atom 
        # "Specifies the name of the Shape. In this case is arc.".
%%---------------------------------------------------------------------

tcl_name(arc).

%creation_options([" ",X1," ",Y1," ",X2," ",Y2," ",'-start',A," "|Other]) :-
%creation_options([" ",X1," ",Y1," ",X2," ",Y2,' -start',A," "|Other]) :-
creation_options([X1,Y1,X2,Y2,min(start),A,min(style),S|Other]) :-
        coord(X,Y,W,H),
        W2 is W / 2,
        H2 is H / 2,
        X1 is X-W2,
        X2 is X+W2,
        Y1 is Y-H2,
        Y2 is Y+H2,
        angle(A),
        style(S),
        inherited creation_options(Other).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

arc_class.
arc_class(Owner) :-
        shape_class(Owner).

arc_class((X,Y),W,H,Owner) :-
        shape_class(Owner),
        set_width(W),
        set_height(H),
        set_center(X,Y).
%       set_angle(A).

arc_class((X,Y),W,H) :-
        set_width(W),
        set_height(H),
        set_center(X,Y).
%       set_angle(A).

:- set_prolog_flag(multi_arity_warnings,on).
