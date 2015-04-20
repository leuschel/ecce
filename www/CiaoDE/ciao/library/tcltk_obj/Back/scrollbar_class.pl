%%---------------------------------------------------------------------
%%
%% SCROLLBAR CLASS
%%
%%---------------------------------------------------------------------

:- class(scrollbar_class,[],[objects]).

:- use_class(library('tcltk_obj/window_class')).

%%
%%
:- data        orient/1.
:- inheritable orient/1.

orient('vertical').

:- export(set_orient/1).

set_orient(Orient) :-
	display('pepepe'),nl,
	atom(Orient),
        set_fact(orient(Orient)),
	notify_changes.

:- export([get_orient/1]).

get_orient(Orient) :-
	orient(Orient).

:- data        width/1.
:- inheritable width/1.

width('5').

:- export(set_width/1).

set_width(Width) :-
	display('set'),nl,
	atom(Width),
        set_fact(width(Width)),
	notify_changes.

:- export([get_width/1]).

get_width(Width) :-
	width(Width).


%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1,creation_position/1,creation_bind/1]).

tcl_name(scrollbar):- display('ppppp').

creation_options([' ',min(width),W,min(orient),O]) :-
	display('eeeee'),nl,
	orient(O),
	width(W).

creation_position([' ']).
creation_bind([' ']).
%creation_position([' ',min(side),S,min(expand),E,min(fill),F,min(padx),X,min(pady),Y]) :-
%	side(S),
%	expand(E),
%	fill(F),
%	padx(X),
%	pady(Y).

%creation_bind([' ',Eventtype,br(C)]) :-
%	event_type(Eventtype),
%display('Antes variables'),nl,
%	variables(V),
%display(V),
%	action(C).

:- inheritable(notify_changes/0).

notify_changes:-
	display('jdfjsd'),
	self(Scroll),
	owner(AnOwner),
	AnOwner instance_of window_class,
	AnOwner:item_changed(Scroll),
	fail.
notify_changes.

:- export([add_owner/1,remove_owner/1]).

add_owner(Owner) :-
	\+ owner(Owner),
	Owner instance_of window_class,
	assertz_fact(owner(Owner)),
	self(Scroll),
	Owner:add_item(Scroll),
	!.
add_owner(_).


remove_owner(Owner) :-
	retract_fact(owner(Owner)),
	Owner instance_of window_class,
	self(Scroll),
	Owner:remove_item(Scroll),
	!.

remove_owner(_).

%%---------------------------------------------------------------------
%% CONSTRUCTOR/DESTRUCTOR
%%---------------------------------------------------------------------

:- data        owner/1.
:- inheritable owner/1.


:- set_prolog_flag(multi_arity_warnings,off).

scrollbar_class.
scrollbar_class(AnOwner) :-
	!,
	scrollbar_class(AnOwner).

:- set_prolog_flag(multi_arity_warnings,on).

destructor :-
	self(Scroll),
	retract_fact(owner(AnOwner)),
	AnOwner instance_of window_class,     % Owner is still alive
	AnOwner:remove_item(Scroll),
	fail.
