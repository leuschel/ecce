%%---------------------------------------------------------------------
%%
%% SHAPE CLASS
%%
%%---------------------------------------------------------------------

:- module(shape_class_doc,[],[objects,assertions,isomodes,regtypes]).

:- use_class(library('tcltk/examples/class/canvas_class')).

%%---------------------------------------------------------------------
%% COLOR 
%%---------------------------------------------------------------------

:- data         tcl_color/2.
:- inheritable  tcl_color/2.

tcl_color(black,black).  % default color.

:- export(get_color/2).
%%---------------------------------------------------------------------
:- pred get_color(+Foreground,+Background) :: atom * atom
        # "Gets the Foreground an Background color.".
%%---------------------------------------------------------------------

get_color(Foreground,Background) :-
        !,
        tcl_color(Foreground,Background).

:- export(set_fg_color/1).
%%---------------------------------------------------------------------
:- pred set_fg_color(+Background) :: atom 
        # "Fill  the  region of the arc with color. If
           color  is  an empty string (the default), then then
           the arc will not be filled.".
%%---------------------------------------------------------------------

set_fg_color(Color) :-
        atom(Color),
        retract_fact(tcl_color(_,BG)),
        asserta_fact(tcl_color(Color,BG)),
        notify_changes.

:- export(set_bg_color/1).
%%---------------------------------------------------------------------
:- pred set_bg_color(+Foreground) :: atom 
        # "Color specifies a color  to  use  for  drawing  the
              arc's  outline.  This option  defaults  to
              black.   If  color  is specified as an empty string
              then no outline is drawn for the arc.".
%%---------------------------------------------------------------------

set_bg_color(Color) :-
        atom(Color),
        retract_fact(tcl_color(FG,_)),
        asserta_fact(tcl_color(FG,Color)),
        notify_changes.

%%---------------------------------------------------------------------
%% BORDER WIDTH
%%---------------------------------------------------------------------

:- data        border/1.
:- inheritable border/1.

border(1).

:- export(set_border_width/1).
%%---------------------------------------------------------------------
:- pred set_border_width(+Border) :: int
        # "Specifies the width of  the  outline  to  be  drawn
              around  the  arc's  region,  in  any  of  the forms
              described in the COORDINATES section above. Wide  outlines
              will  be  drawn centered on the edges of the
              arc's region. This option defaults to 1.0.".
%%---------------------------------------------------------------------

set_border_width(Border) :-
        number(Border),
        Border > 0,
        set_fact(border(Border)),
        notify_changes.

:- export(get_border_width/1).
%%---------------------------------------------------------------------
:- pred get_border_width(-Border) :: int
        # "Gets the value of the width of  the  outline.".
%%---------------------------------------------------------------------

get_border_width(Border) :-
        border(Border).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

tcl_name(_) :- fail.

creation_options([min(fill),BG,min(outline),FG,min(width),B]) :-
        tcl_color(FG,BG),
        border(B).

:- inheritable(notify_changes/0).

notify_changes:-
        self(Shape),
        owner(AnOwner),
        AnOwner instance_of canvas_class,
        AnOwner:item_changed(Shape),
        fail.
notify_changes.

:- export([add_owner/1,remove_owner/1]).

add_owner(Owner) :-
        \+ owner(Owner),
        Owner instance_of canvas_class,
        assertz_fact(owner(Owner)),
        self(Shape),
        Owner:add_item(Shape),
        !.
add_owner(_).


remove_owner(Owner) :-
        retract_fact(owner(Owner)),
        Owner instance_of canvas_class,
        self(Shape),
        Owner:remove_item(Shape),
        !.

remove_owner(_).

%%---------------------------------------------------------------------
%% CONSTRUCTOR/DESTRUCTOR
%%---------------------------------------------------------------------

:- data        owner/1.
:- inheritable owner/1.

:- set_prolog_flag(multi_arity_warnings,off).

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
        AnOwner:remove_item(Shape),
        fail.
