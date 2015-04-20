%%-------------------------------------------------------------------------
%% O'CIAO
%%
%% BASE PERSISTENCY SUPPORT
%%
%%-------------------------------------------------------------------------

:- class(persistent,[],[objects]).

:- use_class(library('class/library/persistent_streamer')).

%%-------------------------------------------------------------------------

:- virtual     persistent_attribute/1.
:- inheritable persistent_attribute/1.

persistent_attribute(_) :-
	fail.
%%-------------------------------------------------------------------------

:- virtual     persistent_state_loaded/0.

persistent_state_loaded.

%%-------------------------------------------------------------------------
%% WRITE PERSISTENT STATE
%%-------------------------------------------------------------------------

:- virtual write_persistent_data/1.
:- export(write_persistent_data/1).

write_persistent_data(Writer) :-
	Writer interface persistent_streamer,
	self(Me),
	arg(1,Me,ID),
	Writer:write_term('$start_object$'(Me)),
	persistent_attribute(Attr),
	write_data_of(ID,Attr,Writer),
	fail.
write_persistent_data(Writer) :-
	Writer interface persistent_streamer,
	self(Me),
	Writer:write_term('$end_object$'(Me)),
	Writer:flush.

write_data_of(ID,Attr,Writer) :-
	functor(Attr,_,A),
	current_attr(Attr,ID),
	plain(A,Attr,Writer),
	Writer:write_term(Attr),
	fail.
write_data_of(_,_,_).

plain(0,_,_) :- !.
plain(N,Attr,Writer) :-
	arg(N,Attr,Arg),
	Arg instance_of persistent,
	!,
	( Arg:write_persistent_data(Writer) -> true ; true ),
	M is N-1,
	plain(M,Attr,Writer).
plain(N,Attr,Writer) :-
	M is N-1,
	plain(M,Attr,Writer).

%%-------------------------------------------------------------------------
%% READ PERSISTENT STATE
%%-------------------------------------------------------------------------

:- virtual read_persistent_data/1.
:- export(read_persistent_data/1).

:- data must_translate/2.

read_persistent_data(Reader) :-
	Reader interface persistent_streamer,
	retractall_fact(must_translate(_,_)),
	self(Me),
	arg(1,Me,ID),
	Reader:current_term('$start_object$'(OldObj)),
	!,
	asserta_fact(must_translate(OldObj,_)),
	read_aux(Reader,ID),
	retractall_fact(must_translate(_,_)),
	( persistent_state_loaded -> true ; true ).
read_persistent_data(_).

read_aux(_,ID) :-
	persistent_attribute(Attr),
	retractall_attr(Attr,ID),
	fail.
read_aux(Reader,ID) :-
	Reader:next_term,
	Reader:current_term(Action),
	dispatch_action(Action,Reader,ID).
read_aux(_,_).

dispatch_action('$start_object$'(OldObj),Reader,_) :-
	functor(OldObj,Class,1),
	NewObj new Class,
	arg(1,NewObj,NewID),
	assertz_fact(must_translate(OldObj,NewObj)),
	read_aux(Reader,NewID),
	!,
	fail.

dispatch_action('$end_object$'(_),_,_) :- !.

dispatch_action(ReadTerm,_,ID) :-
	functor(ReadTerm,F,A),
	functor(Term,F,A),
	apply_translation(A,ReadTerm,Term),
	!,
	assertz_attr(Term,ID),
	fail.

apply_translation(0,_,_) :- !.

apply_translation(N,T1,T2) :-
	arg(N,T1,Arg),
	( must_translate(Arg,NewArg) -> true ; NewArg=Arg ),
	arg(N,T2,NewArg),
	!,
	M is N-1,
	apply_translation(M,T1,T2).

%%-------------------------------------------------------------------------
%% CONSTRUCTOR / DESTRUCTOR
%%-------------------------------------------------------------------------

:- inheritable pers_streamer/1.
:- data pers_streamer/1.

persistent(PersConstructor) :-
	At new PersConstructor,
	( At interface persistent_streamer -> true ; destroy(At) ),
	asserta_fact(pers_streamer(At)),
	At:start_reading,
	read_persistent_data(At),
	At:end_reading,
	!.
persistent(_).

destructor :-
	pers_streamer(At),
	At:start_writing,
	write_persistent_data(At),
	At:end_writing,
	destroy(At).
