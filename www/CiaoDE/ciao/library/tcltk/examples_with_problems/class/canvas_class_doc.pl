%%---------------------------------------------------------------------
%%
%% TCL CANVAS WIDGET
%%
%%---------------------------------------------------------------------

:- module(canvas_class_doc,[],[objects,assertions,isomodes,regtypes]).

:- use_class(library('tcltk/examples/class/shape_class')).

:- use_module(library(system)).
:- use_module(library(strings)).
:- use_module(library(lists),[append/3]).
:- use_module(library('tcltk/tcltk')).
:- use_module(library('tcltk/tcltk_low_level')).

%%----------------------------------------------------------------------

:- comment(title, "The Tcl/Tk Geometry Class Interface").

:- comment(author,"Montse Iglesias").

:- comment(copyright,"@include{Copyright.Manuals}").

:- comment(summary,
        "This section will explain how to use the geometry class using the 
          tcl/tk library ").

:- comment(module,"The @lib{canvas_class} permits create a geometry class using
        tcltk library. The constructor class is canvas_class. ").

:- regtype shape(S) # "@var{S} is a reference to one type of the items
        supported by canvas widgets.".

:- comment(shape/1,"Each item type is characterized by two things: 
        first, the form of the create  command
       used  to  create instances of the type;  and second, a set
       of configuration options for items of that type, which may
       be  used  in the create and itemconfigure widget commands.").

shape(_).
:- regtype option(O) # "@var{O} is @em{hidden} if the Shape is not visible or 
        @em{shown} if its visible.".
option(_).
%%----------------------------------------------------------------------

:- export(item/2).
:- export(interp/1).

:- pred item(+Shape,+Option) :: shape * option
        # "Indicates if the shape is visible or not.".

item(_,_).

:- pred interp(-TclInterpreter) :: tclInterpreter
        # "Creates the TclInterpreter.".
interp(_).
%%----------------------------------------------------------------------

%%---------------------------------------------------------------------
%% CONSTRUCTOR/DESTRUCTOR
%%---------------------------------------------------------------------
:- set_prolog_flag(multi_arity_warnings,off).

%%------------------------------------------------------------------------
:- export(canvas_class/0).

:- pred canvas_class :: list
        #" Creates a new interpreter, asserting the predicate 
        @em{interp(I)}, and the canvas object.".

:- export(canvas_class/1).

:- pred canvas_class(+ItemsList) :: list

        # "Adds items of the list to the canvas object.".
%%------------------------------------------------------------------------

canvas_class :-
        tcl_new(I),
        asserta_fact(interp(I)),
        self_codes(ID),
        append(".",ID,Var),
%       atom_concat('.canvas',ID,Var),
        atom_codes(Var1,Var),
        tcl_eval(I,[canvas, Var1],_),
        tcl_eval(I,[pack, Var],_).

canvas_class([]) :-
        canvas_class.

canvas_class([Item|Next]) :-
        ( add_item(Item) ; true ),
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
%       command(["destroy .canvas",Canvas),
        retract_fact(item(Shape,_)),
        Shape:remove_owner(Canvas),
        fail.
destructor :-
        interp(I),
        tcl_delete(I),
        retract_fact(interp(_)).

%%---------------------------------------------------------------------
%% ADD/REMOVE ITEMS
%%---------------------------------------------------------------------

:- export(add_item/1).
:- export(remove_item/1).
:- export(item_changed/1).

%%------------------------------------------------------------------------
:- pred add_item(+Shape) :: shape

        # "Adds shapes to the canvas object.".
%%------------------------------------------------------------------------

add_item(Shape) :-
        \+ item(Shape,_),
        Shape instance_of shape_class,
        assertz_fact(item(Shape,hidden)),
        self(Canvas),
        Shape:add_owner(Canvas),
        !.
add_item(_).
%%------------------------------------------------------------------------
:- pred remove_item(+Shape) :: shape

        # "Removes shape from the canvas object.".
%%------------------------------------------------------------------------

remove_item(Shape) :-
        hide_item(Shape),
        retract_fact(item(Shape,_)),
        Shape instance_of shape_class,
        self(Canvas),
        Shape:remove_owner(Canvas),
        !.
remove_item(_).

%%------------------------------------------------------------------------
:- pred item_changed(+Shape) :: shape

        # "Removes shape from the canvas object and creates a new one.".
%%------------------------------------------------------------------------

item_changed(Shape) :-
        hide_item(Shape),
        show_item(Shape).

%%---------------------------------------------------------------------
%% SHOW / HIDE ENTIRE CANVAS
%%---------------------------------------------------------------------

:- export(show/0).
%%------------------------------------------------------------------------
:- pred show

        # "Adds shapes to the canvas object.".
%%------------------------------------------------------------------------

show :-
        item(Shape,hidden),
        display('Principio show'),nl,
        show_item(Shape),
        fail.
show.

:- export(hide/0).
%%------------------------------------------------------------------------
:- pred hide

        # "Removes shapes from the canvas object.".
%%------------------------------------------------------------------------

hide :-
        item(Shape,shown),
        hide_item(Shape),
        fail.
hide.

%%---------------------------------------------------------------------
%% SHOW / HIDE SPECIFIC ITEMS
%%---------------------------------------------------------------------

:- export(show_item/1).
%%------------------------------------------------------------------------
:- pred show_item(+Shape) :: shape

        # "Shows Shape into the canvas object.".
%%------------------------------------------------------------------------

show_item(Shape) :-
        display('en el show item de '),display(Shape),nl,
%       self(Canvas),
        self_codes(Canvas),
        item(Shape,hidden),
        Shape instance_of shape_class,
        Shape:tcl_name(ItemName),
        Shape:creation_options(Opts),
        interp(I),
        append(".",Canvas,Var),
        atom_codes(Var1,Var),
        append(Opts,[min(tags)|write(Shape)],Opts1),
        tcl_eval(I,[Var1,' create ',ItemName|Opts1],_),
%       codify([Command," ",Options," -tags tag",Shape],Aux),
        retract_fact(item(Shape,hidden)),
        asserta_fact(item(Shape,shown)).

:- export(hide_item/1).
%%------------------------------------------------------------------------
:- pred hide_item(+Shape) :: shape

        # "Removes Shape from the canvas object.".
%%------------------------------------------------------------------------

hide_item(Shape) :-
        self_codes(Canvas),
        retract_fact(item(Shape,shown)),
        interp(I),
        append(".",Canvas,Var),
        atom_codes(Var1,Var),
%       tcl_eval(I,[Var,'delete tag',Shape],_),
        tcl_eval(I,[Var1,'delete ',write(Shape)],_),
        asserta_fact(item(Shape,hidden)).


%%---------------------------------------------------------------------
%% MACROS
%%---------------------------------------------------------------------

codify([],"").

codify([''|Next],[C,C|CNext]) :-
        !,
        atom_codes('\"',[C]),
        codify(Next,CNext).

codify([[]|Next],[C,C|CNext]) :-
        !,
        atom_codes('\"',[C]),
        codify(Next,CNext).

codify([X|Next],Str) :-
        atom(X),
        !,
        atom_codes(X,XCodes),
        codify(Next,CNext),
        append(XCodes,CNext,Str).

codify([X|Next],Str) :-
        number(X),
        number_codes(X,XCodes),
        !,
        codify(Next,CNext),
        append(XCodes,CNext,Str).

codify([X|Next],Str) :-
        !,
        codify(Next,CNext),
        append(X,CNext,Str).

self_codes(S) :-
        self(Me),
        instance_codes(Me,S).

:- inheritable(self_codes/1).
