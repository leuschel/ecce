%%---------------------------------------------------------------------
%%
%% TCL CANVAS WIDGET
%%
%%---------------------------------------------------------------------

:- class(canvas_class,[],[objects]).

:- use_class(library('tcltk/examples_with_problems/class/shape_class')).

:- use_module(library(system)).
:- use_module(library(strings)).
:- use_module(library(lists),[append/3]).
:- use_module(library('tcltk/tcltk')).
:- use_module(library('tcltk/tcltk_low_level')).


:- data item/2.
:- data interp/1.

%%---------------------------------------------------------------------
%% CONSTRUCTOR/DESTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

canvas_class :-
        tcl_new(I),
        asserta_fact(interp(I)),
        self_codes(ID),
        append(".w",ID,Var),
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

add_item(Shape) :-
        \+ item(Shape,_),
        Shape instance_of shape_class,
        assertz_fact(item(Shape,hidden)),
        self(Canvas),
        Shape:add_owner(Canvas),
        !.
add_item(_).

remove_item(Shape) :-
        hide_item(Shape),
        retract_fact(item(Shape,_)),
        Shape instance_of shape_class,
        self(Canvas),
        Shape:remove_owner(Canvas),
        !.
remove_item(_).


item_changed(Shape) :-
        hide_item(Shape),
        show_item(Shape).

%%---------------------------------------------------------------------
%% SHOW / HIDE ENTIRE CANVAS
%%---------------------------------------------------------------------

:- export(show/0).

show :-
        item(Shape,hidden),
        show_item(Shape),
        fail.
show.

:- export(hide/0).

hide :-
        item(Shape,shown),
        hide_item(Shape),
        fail.
hide.

%%---------------------------------------------------------------------
%% SHOW / HIDE SPECIFIC ITEMS
%%---------------------------------------------------------------------

:- export(show_item/1).

show_item(Shape) :-
%       self(Canvas),
        self_codes(Canvas),
        item(Shape,hidden),
        Shape instance_of shape_class,
        Shape:tcl_name(ItemName),
        Shape:creation_options(Opts),
        interp(I),
        append(".w",Canvas,Var),
        atom_codes(Var1,Var),
        append(Opts,[min(tags)|write(Shape)],Opts1),
        tcl_eval(I,[Var1,' create ',ItemName|Opts1],_),
        retract_fact(item(Shape,hidden)),
        asserta_fact(item(Shape,shown)).

:- export(hide_item/1).

hide_item(Shape) :-
        self_codes(Canvas),
        retract_fact(item(Shape,shown)),
        interp(I),
        append(".w",Canvas,Var),
        atom_codes(Var1,Var),
%       tcl_eval(I,[Var,'delete tag',Shape],_),
        tcl_eval(I,[Var1,'delete ',write(Shape)],_),
        asserta_fact(item(Shape,hidden)).


%%---------------------------------------------------------------------
%% MACROS
%%---------------------------------------------------------------------


self_codes(S) :-
        self(Me),
        instance_codes(Me,S).

:- inheritable(self_codes/1).
