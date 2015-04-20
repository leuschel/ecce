%%---------------------------------------------------------------------
%%
%% TCL WINDOW
%%
%%---------------------------------------------------------------------

:- class(window_class,[],[objects]).

:- use_class(library('tcltk/examples_with_problems/widget_class/widget_class')).

:- use_module(library(system)).
:- use_module(library(strings)).
:- use_module(library(lists),[append/3]).
:- use_module(library('tcltk/tcltk')).
:- use_module(library('tcltk/tcltk_low_level')).
:- use_module(library(aggregates)).

:- data item/2.
:- data interp/1.

:- export(interp/1).
%%---------------------------------------------------------------------
%% CONSTRUCTOR/DESTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

window_class :-
        tcl_new(I),
        asserta_fact(interp(I)),
        tcl_eval(I,[wm,withdraw,'.'],_).
%       self(Id),
%       X = '.',
%       tcl_eval(I,[toplevel,write(X),write(Id),' ',min(background),'gray'],_).

window_class([]) :-
        window_class.

window_class([Item|Next]) :-
        ( add_item(Item) ; true ),
        !,
        window_class(Next).

:- set_prolog_flag(multi_arity_warnings,on).

destructor :-
        self(Principal),
        retract_fact(item(Widget,_)),
        Widget :remove_owner(Principal),
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

add_item(Widget) :-
        \+ item(Widget,_),
        Widget instance_of widget_class,
        assertz_fact(item(Widget,hidden)),
        self(Principal),
        Widget:add_owner(Principal),
        !.
add_item(_).

remove_item(Widget) :-
        hide_item(Widget),
        retract_fact(item(Widget,_)),
        Widget instance_of widget_class,
        self(Principal),
        Widget:remove_owner(Principal),
        !.
remove_item(_).


item_changed(Widget) :-
        hide_item(Widget),
        show_item(Widget).

%%---------------------------------------------------------------------
%% SHOW / HIDE ENTIRE WIDGET
%%---------------------------------------------------------------------

:- export(show/0).

show :-
        self(Id),
        interp(I),
        X = '.',
        tcl_eval(I,[toplevel,write(X),write(Id),' ',min(background),'gray'],_), 
        item(Widget,hidden),
        show_item(Widget),
        fail.
show.

:- export(hide/0).

hide :-
        item(Widget,shown),
        hide_item(Widget),
        fail.
hide.

%%---------------------------------------------------------------------
%% SHOW / HIDE SPECIFIC ITEMS
%%---------------------------------------------------------------------

:- export(show_item/1).

show_item(Widget) :-
        item(Widget,hidden),
        Widget instance_of widget_class,
        Widget:tcl_name(ItemName),
%       atom_concat('.',ItemName,Name),
%       atom_concat(Name,Pr,Var),
%       atom_concat(Name,Widget,Var),
%       append(".",Widget,Name),
%       atom_codes(Var,Name),
        self(ID),
        Widget:creation_options(Opts),
        X = '.',
        append([write(Widget)],Opts,Opts1),
        Widget:creation_position(Post),
        Widget:creation_bind(Bind),
        interp(I),
%       tcl_eval(I,[ItemName,Var|Opts],_),
        tcl_eval(I,[ItemName,write(X),write(ID),write(X)|Opts1],_),
%       tcl_eval(I,[pack,Var|Post],_),
        append([write(Widget)],Post,Post1),
        tcl_eval(I,[pack,write(X),write(ID),write(X)|Post1],_),
%       tcl_eval(I,[bind,Var|Bind],_),
        append([write(Widget)],Bind,Bind1),
        tcl_eval(I,[bind,write(X),write(ID),write(X)|Bind1],_),
        retract_fact(item(Widget,hidden)),
        asserta_fact(item(Widget,shown)).

:- export(hide_item/1).

hide_item(Widget) :-
        self(ID),
        retract_fact(item(Widget,shown)),
        !,
%       retract_fact(item(Widget,_)),
        interp(I),
        X = '.',
        tcl_eval(I,['destroy',write(X),write(ID),write(X)|write(Widget)],_),
        asserta_fact(item(Widget,hidden)).

:- export(set_variable/2).

set_variable(X,Y) :-
        interp(I),
        tcl_eval(I,[set,X,write(Y)],_).


:- export(get_variable/2).

get_variable(X,Y) :-
        interp(I),
        tcl_eval(I,[set,'aux',write(X)],Z),
        number_codes(Y,Z).

:- export(set_title/1).

set_title(X) :-
        interp(I),
        self(ID),
        Y='.',
        tcl_eval(I,[wm,'title',write(Y),write(ID),'',X],_).

:- export(set_maxsize/2).

set_maxsize(X,Y) :-
        interp(I),
        self(ID),
        Z='.',
        tcl_eval(I,[wm,'maxsize',write(Z),write(ID),' ',X,Y],_).

:- export(set_minsize/2).

set_minsize(X,Y) :-
        interp(I),
        self(ID),
        Z='.',
        tcl_eval(I,[wm,'minsize',write(Z),write(ID),'',X,Y],_).

:- export(set_withdraw/0).

set_withdraw :-
        interp(I),
        self(ID),
        Z='.',
        tcl_eval(I,[wm,'withdraw',write(Z),write(ID)],_).

%%-------------------------------------------------------------------
%% TO USE EVENTS
%%-------------------------------------------------------------------

:- export(event_loop/0).

event_loop :-
        interp(I),
        tk_event_loop(I).


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
