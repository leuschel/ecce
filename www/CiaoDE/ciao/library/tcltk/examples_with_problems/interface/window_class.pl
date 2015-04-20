%%---------------------------------------------------------------------
%%
%% TCL WINDOW
%%
%%---------------------------------------------------------------------

:- class(window_class,[],[objects]).

:- use_class(library('tcltk/examples_with_problems/interface/widget_class')).
:- use_class(library('tcltk/examples_with_problems/interface/menu_class')).


:- use_module(library(system)).
:- use_module(library(strings)).
:- use_module(library(lists),[append/3]).
:- use_module(library('tcltk/tcltk')).
:- use_module(library('tcltk/tcltk_low_level')).
:- use_module(library(aggregates)).

:- data item/2.
:- data menu/2.
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

window_class([],[]) :-
        window_class.

window_class([],[Menu|Next]) :-
        ( add_menu(Menu) ; true ),
        !,
        window_class([],Next).

window_class([Item|Next],Menu) :-
        ( add_item(Item) ; true ),
        !,
        window_class(Next,Menu).

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
       display('En el item_changed'),nl,
        show_item(Widget).

%%---------------------------------------------------------------------
%% ADD/REMOVE MENUS
%%---------------------------------------------------------------------

:- export(add_menu/1).
:- export(remove_menu/1).
:- export(menu_changed/1).

add_menu(Menu) :-
%       display('add_menu'),nl,
        \+ menu(Menu,_),
        Menu instance_of menu_class,
        assertz_fact(menu(Menu,hidden)),
        self(Principal),
        Menu:add_owner(Principal),
        !.
add_menu(_).

remove_menu(Menu) :-
%       hide_menu(Menu),
        retract_fact(menu(Menu,_)),
        Menu instance_of menu_class,
        self(Principal),
        Menu:remove_owner(Principal),
        !.
remove_menu(_).


menu_changed(Menu) :-
%       display('En el menu_change'),nl,
        hide_menu(Menu),
        show_menu(Menu).
%       hide_menu_entry(Menu),
%       show_menu_entry(Menu).

%%---------------------------------------------------------------------
%% SHOW / HIDE ENTIRE WIDGET
%%---------------------------------------------------------------------

:- export(show/0).

show :-
%       display('En el show'),nl,
        self(Id),
        interp(I),
        X = '.',
%       tcl_eval(I,[toplevel,write(X),write(Id),' ',min(background),'gray'],_), 
        tcl_eval(I,[wm,deiconify,write(X),write(Id)],_), 
        item(Widget,hidden),
%       display('__________w'),display(Widget),nl,
        show_item(Widget),
        menu(Menu,hidden),
%       display('__________m'),display(Menu),nl,
        show_menu(Menu),
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
       display('En el show item'),nl,
        Widget instance_of widget_class,
        Widget:tcl_name(ItemName),
%       atom_concat('.',ItemName,Name),
%       atom_concat(Name,Pr,Var),
%       atom_concat(Name,Widget,Var),
%        append(".",Widget,Name),
%       atom_codes(Var,Name),
        self(ID),
        Widget:creation_options(Opts),
        X = '.',
        append([write(Widget)],Opts,Opts1),
        Widget:creation_position(Post),
        Widget:creation_bind(Bind),
        interp(I),
%       tcl_eval(I,[ItemName,Var|Opts],_),
       display('En el eval'),nl,
%       display(I),nl,
        tcl_eval(I,[ItemName,write(X),write(ID),write(X)|Opts1],_),
%       tcl_eval(I,[pack,Var|Post],_),
        append([write(Widget)],Post,Post1),
        tcl_eval(I,[grid,write(X),write(ID),write(X)|Post1],_),
%       tcl_eval(I,[bind,Var|Bind],_),
        append([write(Widget)],Bind,Bind1),
        tcl_eval(I,[bind,write(X),write(ID),write(X)|Bind1],_),
        retract_fact(item(Widget,hidden)),
        asserta_fact(item(Widget,shown)).

:- export(hide_item/1).

hide_item(Widget) :-
        self(ID),
	display(Widget),nl,
        retract_fact(item(Widget,shown)),
        !,
%       retract_fact(item(Widget,_)),
        interp(I),
        X = '.',
        tcl_eval(I,['destroy',write(X),write(ID),write(X)|write(Widget)],_),
	display('En el hide item2'),nl,
        asserta_fact(item(Widget,hidden)).

%%---------------------------------------------------------------------
%% SHOW / HIDE SPECIFIC MENUS
%%---------------------------------------------------------------------

%:- export(show_menu_entry/1).
:- export(show_menu/1).

show_menu(Menu):-
%       display('En el show_menu'),nl,
        menu(Menu,hidden),
        Menu instance_of menu_class,
        Menu:tcl_name(ItemName),
        Menu:creation_options(Opts),
        interp(I),
        tcl_eval(I,[ItemName,''|Opts],_),
        retract_fact(menu(Menu,hidden)),
        asserta_fact(menu(Menu,shown)).
%       display('Fin menu_creation'),nl.

%show_menu_entry(Menu) :-
%       display('En el show_menu_entry'),nl,
%       menu(Menu,hidden),
%       display('En el show item'),nl,
%       Menu instance_of menu_class,
%       self(ID),
%       Menu:creation_options_entry(Opts),
%       X = '.',
%       append([write(Widget)],Opts,Opts1),
%       Widget:creation_position(Post),
%       Widget:creation_bind(Bind),
%       interp(I),
%       tcl_eval(I,[Opts],_),
%       tcl_eval(I,[pack,Var|Post],_),
%        append([write(Widget)],Post,Post1),
%       tcl_eval(I,[pack,write(X),write(ID),write(X)|Post1],_),
%       tcl_eval(I,[bind,Var|Bind],_),
%        append([write(Widget)],Bind,Bind1),
%       tcl_eval(I,[bind,write(X),write(ID),write(X)|Bind1],_),
%       retract_fact(menu(Menu,hidden)),
%       asserta_fact(item(Menu,shown)).

%:- export(hide_menu_entry/1).
%:- export(hide_menu/1).

%hide_menu_entry(Menu) :-
%       self(ID),
%       retract_fact(menu(Menu,shown)),
%       asserta_fact(menu(Menu,hidden)).

hide_menu(Menu) :-
        self(ID),
        retract_fact(menu(Menu,shown)),
        !,
        interp(I),
        X = '.',
        tcl_eval(I,['destroy',write(X),write(ID),write(X)|write(Menu)],_),
        asserta_fact(item(Menu,hidden)).

%%
%%
%%
%:- export(set_variable/2).

%set_variable(X,Y) :-
%       interp(I),
%       tcl_eval(I,[set,X,write(Y)],_).


%:- export(get_variable/2).

%get_variable(X,Y) :-
%       interp(I),
%       tcl_eval(I,[set,'aux',write(X)],Z),
%       number_codes(Y,Z).

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
