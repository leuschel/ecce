%%---------------------------------------------------------------------
%%
%% ARC CLASS
%%
%%---------------------------------------------------------------------

:- module(arc_class_doc,[],[objects,assertions,isomodes,regtypes]).

%:- inherit_class(library('tcltk/examples/interface/shape_class')).
%:- implements(library('class/examples/geometry/mobile')).

:- comment(hide,'class$call'/3).
:- comment(hide,'class$used'/2).
%%---------------------------------------------------------------------
%% COORDINATES
%%---------------------------------------------------------------------


coord(0,0,0,0).

%%---------------------------------------------------------------------
:- export(coord/4).

:- pred coord(+X1,+Y1,+X2,+Y2) :: int * int * int *int
        # "@var{X1}, @var{Y1}, @var{X2}, and @var{Y2} give the coordinates of
       two diagonally opposite corners of  a  rectangular  region
       enclosing  the oval that defines the arc.".
%%---------------------------------------------------------------------
:- export([set_width/1,set_height/1,set_center/2]).

%%---------------------------------------------------------------------
:- pred set_width(+Width) :: int
        # "Specifies shape's @var{Width}.".
%%---------------------------------------------------------------------

set_width(W) :-
        number(W),
        W >= 0,
        retract_fact(coord(X,Y,_,H)),
        asserta_fact(coord(X,Y,W,H)),
        notify_changes.

%%---------------------------------------------------------------------
:- pred set_height(+Height) :: int
        # "Specifies shape's @var{Height}.".
%%---------------------------------------------------------------------

set_height(H) :-
        number(H),
        H >= 0,
        retract_fact(coord(X,Y,W,_)),
        asserta_fact(coord(X,Y,W,H)),
        notify_changes.

%%---------------------------------------------------------------------
:- pred set_center(+X,+Y) :: int * int
        # "Specifies shape's center with @var{X} and @var{Y}.".
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
        # "Gets shape's @var{Width}.".
%%---------------------------------------------------------------------
get_width(W) :-
        coord(_,_,W,_).

%%---------------------------------------------------------------------
:- pred get_height(-Height) :: int
        # "Gets shape's @var{Height}.".
%%---------------------------------------------------------------------
get_height(H) :-
        coord(_,_,_,H).

%%---------------------------------------------------------------------
:- pred get_center(-X,-Y) :: int * int
        # "Gets shape's center with @var{X} and @var{Y}.".
%%---------------------------------------------------------------------
get_center(X,Y) :-
        coord(X,Y,_,_).


angle(0).

:- export(set_angle_start/1).

%%---------------------------------------------------------------------
:- pred set_angle_start(+Angle) :: int
        # "@var{Angle} specifies  the beginning of the angular range occupied by 
           the arc.  
           Degrees are given in  units  of
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
        # "Gets the value of the @var{Angle}.".
%%---------------------------------------------------------------------
get_angle_start(Angle) :-
        angle(Angle).

style('pieslice').

:- export(set_style/1).

%%---------------------------------------------------------------------
:- pred set_style(+Style) :: atom 
        # "@var{Style} specifies how to draw the arc. If type is pieslice
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
        # "Gets the @var{Style} of the arc.".
%%---------------------------------------------------------------------
get_style(Style) :-
        style(Style).

%%---------------------------------------------------------------------
%% COLOR 
%%---------------------------------------------------------------------


outline(black).  % default color.

:- export(set_outline/1).

%%---------------------------------------------------------------------
:- pred set_outline(+Color) :: atom
        # "@var{Color} specifies the color used for drawing the arc's outline. This option defaults to black.".
%%---------------------------------------------------------------------
set_outline(Outline) :-
        atom(Outline),
        retract_fact(outline(_)),
        asserta_fact(outline(Outline)),
        notify_changes.

:- export(get_outline/1).

%%---------------------------------------------------------------------
:- pred get_outline(-Color) :: atom
        # "It gets arc's outline @var{Color}.".
%%---------------------------------------------------------------------
get_outline(Outline) :-
        !,
        outline(Outline).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

:- comment(hide,tcl_name/1).
%%---------------------------------------------------------------------
:- pred tcl_name(+Shape) :: atom 
        # "Specifies the name of the @var{Shape}. In this case is arc.".
%%---------------------------------------------------------------------
tcl_name(arc).

%%---------------------------------------------------------------------
:- pred creation_options(-OptionsList) :: list
        # "Creates a list with the options supported by the arc.".
%%---------------------------------------------------------------------
%creation_options([" ",X1," ",Y1," ",X2," ",Y2," ",'-start',A," "|Other]) :-
%creation_options([" ",X1," ",Y1," ",X2," ",Y2,' -start',A," "|Other]) :-
creation_options([X1,Y1,X2,Y2,min(start),A,min(style),S,min(outline),O|Other]) :-
        coord(X,Y,W,H),
        W2 is W / 2,
        H2 is H / 2,
        X1 is X-W2,
        X2 is X+W2,
        Y1 is Y-H2,
        Y2 is Y+H2,
        angle(A),
        style(S),
        outline(O),
        creation_options(Other).

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
