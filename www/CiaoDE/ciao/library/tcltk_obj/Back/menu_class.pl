%%---------------------------------------------------------------------
%%
%% MENU CLASS
%%
%%---------------------------------------------------------------------

:- class(menu_class,[],[objects]).

:- use_class(library('tcltk_obj/window_class')).
:- use_class(library('tcltk_obj/menu_entry_class')).

:- use_module(library('tcltk/tcltk')).
:- use_module(library('tcltk/tcltk_low_level')).
:- use_module(library(lists),[append/3]).

:- data menuentry/2.

:- data        name/1.
:- inheritable name/1.

name('').

:- export(set_name/1).

set_name(Name) :-
%	atom(Name),
	set_fact(name(Name)),
	notify_changes.

:- data        menu/1.
:- inheritable menu/1.

menu('').

:- export(set_menu/1).

set_menu(Menu) :-
	atom(Menu),
	set_fact(menu(Menu)),
	notify_changes.

:- export([get_menu/1]).

get_menu(Menu) :-
	menu(Menu).

:- data        label/1.
:- inheritable label/1.

label('').

:- export(set_label/1).

set_label(Label) :-
	atom(Label),
	set_fact(label(Label)),
	notify_changes.

:- export([get_label/1]).

get_label(Label) :-
	label(Label).

:- data        tearoff/1.
:- inheritable tearoff/1.

tearoff('1').

:- export(set_tearoff/1).

set_tearoff(Tearoff) :-
	atom(Tearoff),
	set_fact(tearoff(Tearoff)),
	notify_changes.

:- export([get_tearoff/1]).

get_tearoff(Tearoff) :-
	tearoff(Tearoff).

list_name([],[]).

list_name([Des|Next],[write(X),write(Des)|Next1]) :-
	X='.',
	list_name(Next,Next1).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1,creation_options_entry/1,creation_menu_name/1]).

tcl_name('menu').

%creation_options([write(X),write(W),write(X),write(MB),write(X),write(N),' ',min(tearoff),T]) :-
creation_options(Opts) :-
%	name([W,MB,N]),
	name(P),
	list_name(P,C),
%	X='.',
%	name(N),
	tearoff(T),
	append(C,[' '],Opts0),
	append(Opts0,[min(tearoff)],Opts1),
	append(Opts1,[T],Opts).

creation_menu_name([write(X),write(W),write(X),write(MB),write(X),write(N)]) :-
	name([W,MB,N]),
	X='.'.


%creation_options([write('.'),write(Widget),write('.'),write(C),' add command',min(label),label(L)|Other]) :-
creation_options_entry([write(X),write(W),write(X),write(MB),write(X),write(N),' add command',min(label),label(L)]) :-
	name([W,MB,N]),
	X='.',
	label(L).
%	inherited creation_options(Other).

:- inheritable(notify_changes/0).

notify_changes:-
	self(Menu),
	owner(AnOwner),
	AnOwner instance_of window_class,
	AnOwner:menu_changed(Menu),
	fail.
notify_changes.


:- export([add_owner/1,remove_owner/1]).

add_owner(Owner) :-
	\+ owner(Owner),
	Owner instance_of window_class,
	assertz_fact(owner(Owner)),
	self(Menu),
	Owner:add_menu(Menu),
	!.
add_owner(_).


remove_owner(Owner) :-
	retract_fact(owner(Owner)),
	Owner instance_of window_class,
	self(Menu),
	Owner:remove_menu(Menu),
	!.

remove_owner(_).



%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------
:- data        owner/1.
:- inheritable owner/1.

:- set_prolog_flag(multi_arity_warnings,off).

menu_class.  % Not owned

menu_class([]):- !.  

menu_class([Menuentry|Next]) :-
	( add_menu_entry(Menuentry) ; true ),
	!,
	menu_class(Next).

%menu_class([]) :- !.

%menu_class([AnOwner|Next]) :-
%	add_owner(AnOwner),
%	!,
%	menu_class(Next).

%menu_class(AnOwner) :-
%	!,
%	add_owner(AnOwner).

:- set_prolog_flag(multi_arity_warnings,on).
%%---------------------------------------------------------------------
%% SHOW / HIDE ENTIRE WIDGET
%%---------------------------------------------------------------------

:- export(show/0).

show :-
	menuentry(Menuentry,hidden),
	show_menu_entry(Menuentry),
	fail.
show.

:- export(hide/0).

hide :-
	menuentry(Menuentry,shown),
	hide_menu_entry(Menuentry),
	fail.
hide.
%%---------------------------------------------------------------------
%% SHOW / HIDE SPECIFIC ITEMS
%%---------------------------------------------------------------------

:- export(show_menu_entry/1).

show_menu_entry(Menuentry) :-
%	menuentry(Menu,hidden),
	Menuentry instance_of menu_entry_class,
	owner(Ow),
	Menuentry:creation_options(Opts),
	Ow:interp(I),
	tcl_eval(I,Opts,_),
	retract_fact(menuentry(Menuentry,hidden)),
	asserta_fact(menuentry(Menuentry,shown)).

:- export(hide_menu_entry/1).

hide_menu_entry(Menuentry) :-
	retract_fact(menuentry(Menuentry,shown)),
	!,
	Menuentry instance_of menu_entry_class,
	owner(Ow),
	Ow:interp(I),
	Menuentry:creation_options_delete(Opts),
	tcl_eval(I,Opts,_),
	asserta_fact(menuentry(Menuentry,hidden)).

%%---------------------------------------------------------------------
%% ADD/REMOVE ITEMS
%%---------------------------------------------------------------------

:- export(add_menu_entry/1).
:- export(remove_menu_entry/1).
:- export(menu_entry_changed/1).

add_menu_entry(Menuentry) :-
	\+ menuentry(Menuentry,_),
	Menuentry instance_of menu_entry_class,
	assertz_fact(menuentry(Menuentry,hidden)),
	self(Principal),
	Menuentry:add_owner(Principal),
	!.
add_menu_entry(_).

remove_menu_entry(Menuentry) :-
	hide_menu_entry(Menuentry),
	retract_fact(menuentry(Menuentry,_)),
	Menuentry instance_of menu_entry_class,
	self(Principal),
	Menuentry:remove_owner(Principal),
	!.
remove_menu_entry(_).


menu_entry_changed(Menuentry) :-
	hide_menu_entry(Menuentry),
	show_menu_entry(Menuentry).
