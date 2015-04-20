%%---------------------------------------------------------------------
%%
%% TCL CANVAS WIDGET
%%
%%---------------------------------------------------------------------

:- module(canvas_class_doc,[],[objects,assertions,isomodes,regtypes]).

:- use_module(library('tcltk/examples/interface/window_class')).
:- use_module(library('tcltk/examples/interface/shape_class')).

:- use_module(library(system)).
:- use_module(library(strings)).
:- use_module(library(lists),[append/3]).
:- use_module(library('tcltk/tcltk')).
:- use_module(library('tcltk/tcltk_low_level')).

%%-----------------------------------------------------------------------

%:- comment(title, "The Tcl/Tk Geometry Class Interface").

%:- comment(author,"Montse Iglesias").

%:- comment(copyright,"@include{Copyright.Manuals}").

%:- comment(summary,
%       "This section will explain how to use the geometry class using the 
%         tcl/tk library ").

%:- comment(module,"The @lib{canvas_class} permits create a geometry class using
%       tcltk library. The constructor class is canvas_class. ").

:- export(shape/1).

:- regtype shape(S) # "@var{S} is a reference to one type of the items
        supported by canvas widgets.".

:- comment(shape/1,"Each item type is characterized in two ways: 
        first, the form of the create  command
       used  to  create instances of the type;  and second, a set
       of configuration options for items of that type, which may
       be  used  in the create and itemconfigure widget commands.").

shape(_).

:- export(option/1).
:- regtype option(O) # "@var{O} is @em{hidden} if the Shape is not visible or 
        @em{shown} if its visible.".
option(_).

%%----------------------------------------------------------------------


%%---------------------------------------------------------------------
%% CONSTRUCTOR/DESTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

%%------------------------------------------------------------------------
:- comment(hide,'class$call'/3).
:- comment(hide,'class$used'/2).
:- export(canvas_class/0).

:- pred canvas_class :: list
        #" Creates a new interpreter, asserting the predicate 
        @em{interp(I)} and the canvas object.".

:- export(canvas_class/1).

:- pred canvas_class(+ItemsList) :: list

        # "Adds items of the list to the canvas object.".
%%------------------------------------------------------------------------
canvas_class. 


canvas_class([]) :-
        canvas_class.

canvas_class([Shape|Next]) :-
        ( add_shape(Shape) ; true ),
        !,
        canvas_class(Next).

:- set_prolog_flag(multi_arity_warnings,on).

%%------------------------------------------------------------------------
:- export(destructor/0).

:- pred destructor

        # "Deletes the shapes of the canvas object and the object.".
%%------------------------------------------------------------------------

destructor :-
        self(Canvas),
        retract_fact(shape(Shape,_)),
        Shape:remove_owner(Canvas),
        fail.

%%---------------------------------------------------------------------
%% ADD/REMOVE ITEMS
%%---------------------------------------------------------------------

:- export(add_shape/1).
:- export(remove_shape/1).
:- export(shape_changed/1).

:- comment(hide,add_shape/1).
:- comment(hide,remove_shape/1).
:- comment(hide,shape_changed/1).
%%------------------------------------------------------------------------
:- pred add_shape(+Shape) :: shape

        # "Adds @var{Shape} to the canvas object.".
%%------------------------------------------------------------------------

add_shape(Shape) :-
        \+ shape(Shape,_),
        Shape instance_of shape_class,
        assertz_fact(shape(Shape,hidden)),
        self(Canvas),
        Shape:add_owner(Canvas),
        !.
add_shape(_).

%%------------------------------------------------------------------------
:- pred remove_shape(+Shape) :: shape

        # "Removes @var{Shape} from the canvas object.".
%%------------------------------------------------------------------------

remove_shape(Shape) :-
        hide_shape(Shape),
        retract_fact(shape(Shape,_)),
        Shape instance_of shape_class,
        self(Canvas),
        Shape:remove_owner(Canvas),
        !.
remove_shape(_).

%%------------------------------------------------------------------------
:- pred shape_changed(+Shape) :: shape

        # "Removes @var{Shape} from the canvas object and creates a new one.".
%%------------------------------------------------------------------------

shape_changed(Shape) :-
        hide_shape(Shape),
        show_shape(Shape).

%%---------------------------------------------------------------------
%% SHOW / HIDE ENTIRE CANVAS
%%---------------------------------------------------------------------

%:- export(show/0).

%%------------------------------------------------------------------------
:- pred show

        # "Shows shapes to the canvas object.".
%%------------------------------------------------------------------------

show :-
        shape(Shape,hidden),
        display('Principio show'),nl,
        show_shape(Shape),
        fail.
show.

%:- export(hide/0).
%%------------------------------------------------------------------------
:- pred hide

        # "Hides shapes from the canvas object.".
%%------------------------------------------------------------------------

hide :-
        shape(Shape,shown),
        hide_shape(Shape),
        fail.
hide.

%%---------------------------------------------------------------------
%% SHOW / HIDE SPECIFIC ITEMS
%%---------------------------------------------------------------------

%:- export(show_shape/1).
%%------------------------------------------------------------------------
:- pred show_shape(+Shape) :: shape

        # "Shows @var{Shape} into the canvas object.".
%%------------------------------------------------------------------------

show_shape(Shape) :-
        display('en el show item de '),display(Shape),nl,
        owner(OW),
        self(Canvas),
        shape(Shape,hidden),
        Shape instance_of shape_class,
        Shape:tcl_name(ItemName),
        Shape:creation_options(Opts),
        OW:interp(I),
        X='.',
        append(Opts,[min(tags)|write(Shape)],Opts1),
        tcl_eval(I,[write(X),write(OW),write(X),write(Canvas),' ',' create ',ItemName|Opts1],_),
        retract_fact(shape(Shape,hidden)),
        asserta_fact(shape(Shape,shown)).

%:- export(hide_shape/1).

%%------------------------------------------------------------------------
:- pred hide_shape(+Shape) :: shape

        # "Hides @var{Shape} from the canvas object.".
%%------------------------------------------------------------------------

hide_shape(Shape) :-
        self(Canvas),
        retract_fact(shape(Shape,shown)),
        owner(OW),
        OW:interp(I),
        X='.',
        tcl_eval(I,[write(X),write(OW),write(X),write(Canvas),' delete ',write(Shape)],_),
        asserta_fact(shape(Shape,hidden)).


:- export([add_owner/1,remove_owner/1]).
:- comment(hide,'add_owner'/1).
:- comment(hide,'remove_owner'/1).

add_owner(Owner) :-
        \+ owner(Owner),
        Owner instance_of window_class,
        assertz_fact(owner(Owner)),
        self(Menu),
        Owner:add_canvas(Menu),
        !.
add_owner(_).


remove_owner(Owner) :-
        retract_fact(owner(Owner)),
        Owner instance_of window_class,
        self(Menu),
        Owner:remove_canvas(Menu),
        !.

remove_owner(_).

%%
%%
%%
%%---------------------------------------------------------------------
%% WIDTH
%%---------------------------------------------------------------------


width(100).

:- export(set_width/1).
%%---------------------------------------------------------------------
:- pred set_width(+Width) :: num
        # "Sets the @var{Width} of the canvas. Default 100.".
%%---------------------------------------------------------------------

set_width(Width) :-
        num(Width),
        set_fact(width(Width)),
        notify_changes.

:- export(get_width/1).
%%---------------------------------------------------------------------
:- pred get_width(-Width) :: num
        # "Gets the @var{Width} of the canvas.".
%%---------------------------------------------------------------------

get_width(Width) :-
        width(Width).

%%---------------------------------------------------------------------
%% HEIGHT
%%---------------------------------------------------------------------


height(50).

:- export(set_height/1).
%%---------------------------------------------------------------------
:- pred set_height(+Height) :: num
        # "Sets the @var{Height} of the canvas. Default 50.".
%%---------------------------------------------------------------------

set_height(Height) :-
        num(Height),
        set_fact(height(Height)),
        notify_changes.

:- export(get_height/1).

%%---------------------------------------------------------------------
:- pred get_height(-Height) :: num
        # "Gets the @var{Height} of the canvas.".
%%---------------------------------------------------------------------
get_height(Height) :-
        height(Height).

%%---------------------------------------------------------------------
%% SIDE
%%---------------------------------------------------------------------


side('top').

:- export(set_side/1).
%%---------------------------------------------------------------------
:- pred set_side(+Side) :: atom
        # "Specifies which @var{Side} of the master, the slave(s) will be packed against. Must be left, right, top or bottom. Defaults to top.".
%%---------------------------------------------------------------------

set_side(Side) :-
        atom(Side),
        set_fact(side(Side)),
        notify_changes.

:- export(get_side/1).
%%---------------------------------------------------------------------
:- pred get_side(-Side) :: atom
        # "Gets the @var{Side} of the canvas.".
%%---------------------------------------------------------------------

get_side(Side) :-
        side(Side).

%%---------------------------------------------------------------------
%% EXPAND
%%---------------------------------------------------------------------


expand('0').

:- export(set_expand/1).
%%---------------------------------------------------------------------
:- pred set_expand(+Value) :: atom
        # "Specifies whether the slaves should be expanded to consume extra space in their master. @var{Value} may have any proper boolean value, such as 1 or no. Defaults to 0".
%%---------------------------------------------------------------------

set_expand(Expand) :-
        atom(Expand),
        set_fact(expand(Expand)),
        notify_changes.

:- export(get_expand/1).
%%---------------------------------------------------------------------
:- pred get_expand(-Value) :: atom
        # "Gets the boolean @var{Value} which indicates if the slaves should be expanded or no.".
%%---------------------------------------------------------------------

get_expand(Expand) :-
        expand(Expand).

%%---------------------------------------------------------------------
%% FILL
%%---------------------------------------------------------------------


fill('none').

:- export(set_fill/1).
%%---------------------------------------------------------------------
:- pred set_fill(+Option) :: atom
        # "If a slave's parcel is larger than its requested dimensions, this @var{Option} may be used to stretch the slave. Style must have one of the following values: none ( this is the default), x, y, both".
%%---------------------------------------------------------------------

set_fill(Fill) :-
        atom(Fill),
        set_fact(fill(Fill)),
        notify_changes.

:- export(get_fill/1).
%%---------------------------------------------------------------------
:- pred get_fill(-Option) :: atom
        # "Gets the fill value of the canvas".
%%---------------------------------------------------------------------

get_fill(Fill) :-
        fill(Fill).

%%---------------------------------------------------------------------
%% PADX
%%---------------------------------------------------------------------


padx('0').

:- export(set_padx/1).
%%---------------------------------------------------------------------
:- pred set_padx(+Amount) :: atom
        # "@var{Amount} specifies how much horizontal external padding to leave on each side of the slave(s). Amount defaults to 0.".
%%---------------------------------------------------------------------

set_padx(Padx) :-
        atom(Padx),
        set_fact(padx(Padx)),
        notify_changes.

:- export(get_padx/1).
%%---------------------------------------------------------------------
:- pred get_padx(-Amount) :: atom
        # "Gets the @var{Amount} which specifies how much horizontal external padding to leave on each side of the slaves.".
%%---------------------------------------------------------------------

get_padx(Padx) :-
        padx(Padx).

%%---------------------------------------------------------------------
%% PADY
%%---------------------------------------------------------------------


pady('0').

:- export(set_pady/1).
%%---------------------------------------------------------------------
:- pred set_pady(+Amount) :: atom
        # "@var{Amount} specifies how much vertical external padding to leave on each side of the slave(s). Amount defaults to 0.".
%%---------------------------------------------------------------------

set_pady(Pady) :-
        atom(Pady),
        set_fact(pady(Pady)),
        notify_changes.

:- export(get_pady/1).
%%---------------------------------------------------------------------
:- pred get_pady(-Amount) :: atom
        # "Gets the @var{Amount} which specifies how much vertical external padding to leave on each side of the slaves.".
%%---------------------------------------------------------------------

get_pady(Pady) :-
        pady(Pady).

:- export([tcl_name/1,creation_options/1,creation_position/1]).

:- comment(hide,tcl_name/1).
tcl_name(_) :- fail.

%%---------------------------------------------------------------------
:- pred creation_options(-OptionsList) :: list
        # "Creates a list with the options supported by the canvas.".
%%---------------------------------------------------------------------

creation_options([min(width),W,min(height),H]) :-
%       background(B),
%       borderwidth(O),
        width(W),
        height(H).
%       highlightbackground(HB),
%       highlightcolor(HC),
%       relief(R).

%%---------------------------------------------------------------------
:- pred creation_position(-OptionsList) :: list
        # "Creates a list with the options supported by the @em{pack} command.".
%%---------------------------------------------------------------------

creation_position([' ',min(side),S,min(expand),E,min(fill),F,min(padx),X,min(pady),Y]) :-
        side(S),
        expand(E),
        fill(F),
        padx(X),
        pady(Y).

%creation_bind([' ',Eventtype,br(C)]) :-
%       event_type(Eventtype),
%       action(C).


notify_changes:-
        display('En el notify_changes canvas'),nl,
        self(Canvas),
        owner(AnOwner),
        AnOwner instance_of window_class,
        AnOwner:canvas_changed(Canvas),
        fail.
notify_changes.
