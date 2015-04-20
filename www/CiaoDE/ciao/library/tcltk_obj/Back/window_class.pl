%%---------------------------------------------------------------------
%%
%% TCL WINDOW
%%
%%---------------------------------------------------------------------

:- class(window_class,[],[objects]).

:- use_class(library('tcltk_obj/widget_class')).
:- use_class(library('tcltk_obj/menu_class')).
:- use_class(library('tcltk_obj/canvas_class')).


:- use_module(library(system)).
:- use_module(library(strings)).
:- use_module(library(lists),[append/3]).
:- use_module(library('tcltk/tcltk')).
:- use_module(library('tcltk/tcltk_low_level')).
:- use_module(library(aggregates)).

:- data item/2.
:- data menu/2.
:- data canvas/2.
:- data interp/1.

:- export(interp/1).
%%---------------------------------------------------------------------
%% CONSTRUCTOR/DESTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).


window_class :-
	tcl_new(I),
	asserta_fact(interp(I)),
	tcl_eval(I,[wm,withdraw,'.'],_),
	self(Id),
	X = '.',
	tcl_eval(I,[toplevel,write(X),write(Id),' ',min(background),'gray'],_),
	tcl_eval(I,[wm,withdraw,write(X),write(Id)],_).

window_class([],[],[]) :-
        display('---- TEST ----'),nl,
	window_class.

window_class([],[],[Canvas|Next]) :-
	( add_canvas(Canvas) ; true ),
	!,
	window_class([],[],Next).

window_class([],[Menu|Next],Canvas) :-
	( add_menu(Menu) ; true ),
	!,
	window_class([],Next,Canvas).

window_class([Item|Next],Menu,Canvas) :-
	( add_item(Item) ; true ),
	!,
	window_class(Next,Menu,Canvas).

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
%% ADD/REMOVE MENUS
%%---------------------------------------------------------------------

:- export(add_menu/1).
:- export(remove_menu/1).
:- export(menu_changed/1).

add_menu(Menu) :-
	\+ menu(Menu,_),
	Menu instance_of menu_class,
	assertz_fact(menu(Menu,hidden)),
	self(Principal),
	Menu:add_owner(Principal),
	!.
add_menu(_).

remove_menu(Menu) :-
%	hide_menu(Menu),
	retract_fact(menu(Menu,_)),
	Menu instance_of menu_class,
	self(Principal),
	Menu:remove_owner(Principal),
	!.
remove_menu(_).


menu_changed(Menu) :-
	hide_menu(Menu),
	show_menu(Menu).

%%---------------------------------------------------------------------
%% ADD/REMOVE CANVAS
%%---------------------------------------------------------------------

:- export(add_canvas/1).
:- export(remove_canvas/1).
:- export(canvas_changed/1).

add_canvas(Canvas) :-
	\+ canvas(Canvas,_),
	Canvas instance_of canvas_class,
	assertz_fact(canvas(Canvas,hidden)),
	self(Principal),
	Canvas:add_owner(Principal),
	!.
add_canvas(_).

remove_canvas(Canvas) :-
	hide_canvas(Canvas),
	retract_fact(canvas(Canvas,_)),
	Canvas instance_of canvas_class,
	self(Principal),
	Canvas:remove_owner(Principal),
	!.
remove_canvas(_).


canvas_changed(Canvas) :-
	hide_canvas(Canvas),
	show_canvas(Canvas).

%%---------------------------------------------------------------------
%% SHOW / HIDE ENTIRE WIDGET
%%---------------------------------------------------------------------

:- export(show/0).

show :-
	self(Id),
	interp(I),
	X = '.',
%	tcl_eval(I,[toplevel,write(X),write(Id),' ',min(background),'gray'],_), 
	tcl_eval(I,[wm,deiconify,write(X),write(Id)],_), 
%	item(Widget,hidden),
	show_item(_),
%	menu(Menu,hidden),
	show_menu(_),
%	canvas(Canvas,hidden),
	show_canvas(_),
	fail.

show.

:- export(hide/0).

hide :-
	item(Widget,shown),
	hide_item(Widget),
	menu(Menu,shown),
	hide_menu(Menu),
	canvas(Canvas,shown),
	hide_canvas(Canvas),
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
	self(ID),
	Widget:creation_options(Opts),
	X = '.',
	append([write(Widget)],Opts,Opts1),
	Widget:creation_position(Post),
%	Widget:creation_position_grid(Post),
	Widget:creation_bind(Bind),
	interp(I),
%	tcl_eval(I,[ItemName,Var|Opts],_),
	tcl_eval(I,[ItemName,write(X),write(ID),write(X)|Opts1],_),
%	tcl_eval(I,[pack,Var|Post],_),
        append([write(Widget)],Post,Post1),
	tcl_eval(I,[pack,write(X),write(ID),write(X)|Post1],_),
%	tcl_eval(I,[grid,write(X),write(ID),write(X)|Post1],_),
%	tcl_eval(I,[bind,Var|Bind],_),
        append([write(Widget)],Bind,Bind1),
	tcl_eval(I,[bind,write(X),write(ID),write(X)|Bind1],_),
	retract_fact(item(Widget,hidden)),
	asserta_fact(item(Widget,shown)),
	fail.

show_item(_).

:- export(hide_item/1).

hide_item(Widget) :-
	self(ID),
	retract_fact(item(Widget,shown)),
	!,
%	retract_fact(item(Widget,_)),
	interp(I),
	X = '.',
	tcl_eval(I,['destroy',write(X),write(ID),write(X)|write(Widget)],_),
	asserta_fact(item(Widget,hidden)).

%%---------------------------------------------------------------------
%% SHOW / HIDE SPECIFIC MENUS
%%---------------------------------------------------------------------

:- export(show_menu/1).

show_menu(Menu):-
	menu(Menu,hidden),
	Menu instance_of menu_class,
	Menu:tcl_name(ItemName),
	Menu:creation_options(Opts),
	interp(I),
	tcl_eval(I,[ItemName,''|Opts],_),
	retract_fact(menu(Menu,hidden)),
	asserta_fact(menu(Menu,shown)),
	fail.

show_menu(_).


hide_menu(Menu) :-
	retract_fact(menu(Menu,shown)),
	!,
	Menu instance_of menu_class,
	Menu:creation_menu_name(Opts),
	interp(I),
%	X = '.',
%	tcl_eval(I,['destroy',write(X),write(ID),write(X)|write(Menu)],_),
	tcl_eval(I,['destroy',''|Opts],_),
	asserta_fact(menu(Menu,hidden)).

%%---------------------------------------------------------------------
%% SHOW / HIDE SPECIFIC MENUS
%%---------------------------------------------------------------------

:- export(show_canvas/1).

show_canvas(Canvas):-
	canvas(Canvas,hidden),
	Canvas instance_of canvas_class,
% 	Canvas:tcl_name(ItemName),
	Canvas:creation_options(Opts),
	Canvas:creation_position(Post),
	interp(I),
	X='.',
	self(ID),
	tcl_eval(I,[canvas,write(X),write(ID),write(X),write(Canvas),' '|Opts],_),
%	tcl_eval(I,[canvas,write(X),write(ID),write(X),write(Canvas)],_),
	tcl_eval(I,[pack,write(X),write(ID),write(X),write(Canvas),' '|Post],_),
	retract_fact(canvas(Canvas,hidden)),
	asserta_fact(canvas(Canvas,shown)),
	fail.

show_canvas(_).

hide_canvas(Canvas) :-
	retract_fact(canvas(Canvas,shown)),
	!,
	self(ID),
	interp(I),
	X = '.',
	tcl_eval(I,['destroy',write(X),write(ID),write(X)|write(Canvas)],_),
	asserta_fact(canvas(Canvas,hidden)).

%%
%%

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
