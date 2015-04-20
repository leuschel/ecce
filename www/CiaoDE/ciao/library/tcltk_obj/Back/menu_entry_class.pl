%%---------------------------------------------------------------------
%%
%% MENU ENTRY CLASS
%%
%%---------------------------------------------------------------------

:- class(menu_entry_class,[],[objects]).

:- use_class(library('tcltk_obj/menu_class')).
:- use_module(library(lists),[append/3]).

:- data        name/1.
:- inheritable name/1.

name('').

:- export(set_name/1).

set_name(Name) :-
%	atom(Name),
	set_fact(name(Name)),
	notify_changes.

:- data        action/1.
:- inheritable action/1.

action('').

:- export(set_action/1).

set_action(Predicate) :-
	atom(Predicate),
	set_fact(action([prolog1,dq(write(execute(Predicate)))])),
	notify_changes.

:- data        label/1.
:- inheritable label/1.

label('default').

:- export(set_label/1).

set_label(Label) :-
	atom(Label),
	set_fact(label(Label)),
	notify_changes.

:- export([get_label/1]).

get_label(Label) :-
	label(Label).

:- data        menu/1.
:- inheritable menu/1.

menu('default').

:- export(set_menu/1).

set_menu(Menu) :-
	atom(menu),
	set_fact(menu(Menu)),
	notify_changes.

:- export([get_menu/1]).

get_menu(Menu) :-
	menu(Menu_entry_name),
	name(Name),
	append(Name,[Menu_entry_name],Menu).

list_name([],[]).

list_name([Des|Next],[write(X),write(Des)|Next1]) :-
	X='.',
	list_name(Next,Next1).


%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1,creation_options_delete/1]).

tcl_name('').

%creation_options([write(X),write(W),write(X),write(MB),write(X),write(N),' add cascade',min(label),L,min(command),br(C),min(menu),write(X),write(W),write(X),write(MB),write(X),write(N),write(X),M]) :-
creation_options(Opts) :-
	name(N),
	list_name(N,NL),
	append(NL,[' add command'],Opts0),
%	X='.',
	label(L),
	append(Opts0,[min(label)],Opts1),
	append(Opts1,[L],Opts2),
	action(C),
	append(Opts2,[min(command)],Opts3),
	append(Opts3,[br(C)],Opts).
%	menu(M),
%	display(M),nl,
%	append(N,[M],NM),
%	list_name(NM,LN),
%	append(Opts4,[min(menu)],Opts5),
%	append(Opts5,LN,Opts),
%	display(Opts),nl.


creation_options_delete([write(X),write(W),write(X),write(MB),write(X),write(N),' delete ','1']) :-
	name([W,MB,N]),
	X='.'.

:- inheritable(notify_changes/0).

notify_changes:-
	self(Menuentry),
	owner(AnOwner),
	AnOwner instance_of menu_class,
	AnOwner:menu_entry_changed(Menuentry),
	fail.

notify_changes.

:- export([add_owner/1,remove_owner/1]).

add_owner(Owner) :-
	\+ owner(Owner),
	Owner instance_of menu_class,
	assertz_fact(owner(Owner)),
	self(Menuentry),
	Owner:add_menu_entry(Menuentry),
	!.
add_owner(_).


remove_owner(Owner) :-
	retract_fact(owner(Owner)),
	Owner instance_of menu_class,
	self(Menuentry),
	Owner:remove_menu_entry(Menuentry),
	!.

remove_owner(_).



%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------
:- data        owner/1.
:- inheritable owner/1.

:- set_prolog_flag(multi_arity_warnings,off).

menu_entry_class.  % Not owned

menu_entry_class([]) :- !.

:- set_prolog_flag(multi_arity_warnings,on).
