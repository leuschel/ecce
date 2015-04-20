%%---------------------------------------------------------------------
%%
%% OVAL CLASS
%%
%%---------------------------------------------------------------------

:- module(oval_class_doc,[],[objects,assertions,isomodes,regtypes]).

%:- inherit_class(library('tcltk/examples/interface/shape_class')).
%:- implements(library('class/examples/geometry/mobile')).
%:- implements(library('class/examples/class/mobile')).

:- comment(hide,'class$call'/3).
:- comment(hide,'class$used'/2).
%%---------------------------------------------------------------------
%% COORDINATES
%%---------------------------------------------------------------------


%%---------------------------------------------------------------------
:- export(coord/4).
:- pred coord(+X1,+Y1,+X2,+Y2) :: int * int * int *int
        # "@var{X1}, @var{Y1}, @var{X2}, and @var{Y2} give the  coordinates  of
       two  diagonally  opposite  corners of a rectangular region
       enclosing the oval.".
%%---------------------------------------------------------------------
coord(0,0,0,0).

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
        # "Specifies shape's @var{Heigh}.".
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

%%---------------------------------------------------------------------
%% COLOR 
%%---------------------------------------------------------------------

outline(black).  % default color.

:- export(set_outline/1).

%%---------------------------------------------------------------------
:- pred set_outline(+Color) :: atom
        # "@var{Color} specifies the color to be used for drawing the oval's outline. This option defaults to black.".
%%---------------------------------------------------------------------
set_outline(Outline) :-
        atom(Outline),
        retract_fact(outline(_)),
        asserta_fact(outline(Outline)),
        notify_changes.

:- export(get_outline/1).

%%---------------------------------------------------------------------
:- pred get_outline(-Color) :: atom
        # "Gets oval's outline @var{Color}.".
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
        # "Specifies the name of the @var{Shape}. In this case is oval.".
%%---------------------------------------------------------------------
tcl_name(oval).

%%---------------------------------------------------------------------
:- pred creation_options(-OptionsList) :: list
        # "Creates a list with the options supported by the oval.".
%%---------------------------------------------------------------------
creation_options([X1,Y1,X2,Y2,min(outline),O|Other]) :-
        coord(X,Y,W,H),
        W2 is W / 2,
        H2 is H / 2,
        X1 is X-W2,
        X2 is X+W2,
        Y1 is Y-H2,
        Y2 is Y+H2,
        outline(O),
        creation_options(Other).

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
