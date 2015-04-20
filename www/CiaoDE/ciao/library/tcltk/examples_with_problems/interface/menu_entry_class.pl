%%---------------------------------------------------------------------
%%
%% MENU ENTRY CLASS
%%
%%---------------------------------------------------------------------

:- class(menu_entry_class,[],[objects]).

:- use_class(library('tcltk/examples_with_problems/interface/menu_class')).

:- data        name/1.
:- inheritable name/1.

name('').

:- export(set_name/1).

set_name(Name) :-
%       atom(Name),
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


%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

tcl_name('').

creation_options([write(X),write(W),write(X),write(MB),write(X),write(N),' add command',min(label),L,min(command),br(C)]) :-
        name([W,MB,N]),
        X='.',
        label(L),
        action(C).

:- inheritable(notify_changes/0).

notify_changes:-
%       display('En el notify changes menu'),nl,
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
