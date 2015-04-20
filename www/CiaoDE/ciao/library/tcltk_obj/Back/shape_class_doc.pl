%%---------------------------------------------------------------------
%%
%% SHAPE CLASS
%%
%%---------------------------------------------------------------------

:- module(shape_class_doc,[],[objects,assertions,isomodes,regtypes]).

:- use_module(library('tcltk_obj/canvas_class')).

:- comment(hide,'class$call'/3).
:- comment(hide,'class$used'/2).
%%---------------------------------------------------------------------
%% COLOR 
%%---------------------------------------------------------------------


tcl_color(black).  % default color.

:- export(set_bg_color/1).
%%---------------------------------------------------------------------
:- pred set_bg_color(+Color) :: atom 
        # "@var{Color} specifies the color  to  use  for  drawing  the
             shape's  outline.  This option  defaults  to
          black. If color is not specified then no outline is drawn for the shape.".
%%---------------------------------------------------------------------

set_bg_color(Color) :-
        atom(Color),
        retract_fact(tcl_color(_)),
        asserta_fact(tcl_color(Color)),
        notify_changes.

:- export(get_color/1).
%%---------------------------------------------------------------------
:- pred get_color(-Background) :: atom 
        # "Gets the shape @var{Background Color}.".
%%---------------------------------------------------------------------

get_color(Background) :-
        !,
        tcl_color(Background).

%:- export(set_fg_color/1).

%set_fg_color(Color) :-
%       atom(Color),
%       retract_fact(tcl_color(_,BG)),
%       asserta_fact(tcl_color(Color,BG)),
%       notify_changes.

%%---------------------------------------------------------------------
%% BORDER WIDTH
%%---------------------------------------------------------------------

border(1).

:- export(set_border_width/1).

%%---------------------------------------------------------------------
:- pred set_border_width(+Width) :: num 
        # "Specifies the @var{Width} borderthat the canvas widget should request from its geometry manager.".
%%---------------------------------------------------------------------
set_border_width(Border) :-
        number(Border),
        Border > 0,
        set_fact(border(Border)),
        notify_changes.

:- export(get_border_width/1).

%%---------------------------------------------------------------------
:- pred get_border_width(-Width) :: num 
        # "Gets the @var{Width} border of the canvas widget.".
%%---------------------------------------------------------------------
get_border_width(Border) :-
        border(Border).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

:- comment(hide,'tcl_name'/1).

tcl_name(_) :- fail.

%%---------------------------------------------------------------------
:- pred creation_options(-OptionsList) :: list
        # "Creates a list with the options supported by the shapes.".
%%---------------------------------------------------------------------
%creation_options([min(fill),BG,min(outline),FG,min(width),B]) :-
creation_options([min(fill),BG,min(width),B]) :-
        tcl_color(BG),
        border(B).


notify_changes:-
        self(Shape),
        owner(AnOwner),
        AnOwner instance_of canvas_class,
        AnOwner:shape_changed(Shape),
        fail.
notify_changes.

:- export([add_owner/1,remove_owner/1]).

:- comment(hide,'add_owner'/1).
:- comment(hide,'remove_owner'/1).

add_owner(Owner) :-
        \+ owner(Owner),
        Owner instance_of canvas_class,
        assertz_fact(owner(Owner)),
        self(Shape),
        Owner:add_shape(Shape),
        !.
add_owner(_).


remove_owner(Owner) :-
        retract_fact(owner(Owner)),
        Owner instance_of canvas_class,
        self(Shape),
        Owner:remove_shape(Shape),
        !.

remove_owner(_).

%%---------------------------------------------------------------------
%% CONSTRUCTOR/DESTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).
%%------------------------------------------------------------------------
:- export(shape_class/0).

:- pred shape_class :: list
        #" Creates a new shape object.".

:- export(shape_class/1).

:- pred shape_class(+ShapeList) :: list

        # "Adds shapes of the list to the canvas object.".
%%------------------------------------------------------------------------

shape_class.  % Not owned

shape_class([]) :- !.

shape_class([AnOwner|Next]) :-
        add_owner(AnOwner),
        !,
        shape_class(Next).

shape_class(AnOwner) :-
        !,
        add_owner(AnOwner).

:- set_prolog_flag(multi_arity_warnings,on).

destructor :-
        self(Shape),
        retract_fact(owner(AnOwner)),
        AnOwner instance_of canvas_class,     % Owner is still alive
        AnOwner:remove_shape(Shape),
        fail.
