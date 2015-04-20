%%transaction_concurrency.pl
%%(c)2003 Nick D. Pattengale
%% nickp@cs.unm.edu

:- module(transaction_concurrency,[write_lock/2,read_lock/2,unlock/1],[assertions,regtypes]).

:- use_module(library(lists), [contains_ro/2,delete/3]).
:- use_module(library(concurrency), [atom_lock_state/2,lock_atom/1,unlock_atom/1]).


:- comment(title,"Shared Read/Write Locks").

:- comment(subtitle, "Providing Support for Strict Two-Phase 
	              Locking with Deadlock Avoidance").

:- comment(copyright, "@copyright 2003 Nick D. Pattengale").

:- comment(author,"Nick D. Pattengale - @email{nickp@@cs.unm.edu}").

:- comment(usage,"This module was written for internal use by the
	   @lib{transactions} module.  Therefore, unless you know
	   what you are doing you should not plan to use this module
	   directly in user code.").

%%the lock routines, as invoked by different threads
%%will cooperate via a lock table.  a request for
%%a lock with either succeed (with the possibility
%%of blocking for a period) indicating that the lock
%%was obtained, or fail indicating that blocking
%%for the lock would result in deadlock and the 
%%transaction needs to be aborted.

:- data(lock_table/3).

%waitfor arc is simply a DAG.  It maps
%transaction dependencies. The existence
%of a cycle would mean deadlock.  This may
%not scale well to millions of interdependent
%transactions.
:- data(waitfor_arc/3).

reachable(V,V).
reachable(V1,V2) :-
	waitfor_arc(V1,V,_),
	reachable(V,V2).

ensure_no_deadlock([],_,_).
ensure_no_deadlock([T1|Td],T2,F/A) :-
	(reachable(T2,T1) ->
	 fail
	;ensure_no_deadlock(Td,T2,F/A),
	 asserta_fact( waitfor_arc(T1,T2,F/A) )).

cleanup_waitfor(F/A, Txn_ID) :-
	(waitfor_arc(X,Txn_ID,F/A) ->
	 retract_fact( waitfor_arc(X,Txn_ID,F/A) ),
	 cleanup_waitfor(F/A,Txn_ID)
	;true).

ensure_block(F/A) :-
	name_arity_to_atom(F/A, Pred_Atom),
	%display_string("ensure_block("),
	%display(Pred_Atom), display_string(")"), nl,
	atom_lock_state(Pred_Atom, Value),
	Value = 1,
	lock_atom(Pred_Atom),
	lock_atom(Pred_Atom).
ensure_block(F/A) :-
	name_arity_to_atom(F/A, Pred_Atom),
	atom_lock_state(Pred_Atom, Value),
	Value = 0,
	lock_atom(Pred_Atom).

ensure_unblock(F/A) :-
	name_arity_to_atom(F/A, Pred_Atom),
	atom_lock_state(Pred_Atom, Value),
	Value = 0,
	unlock_atom(Pred_Atom).
ensure_unblock(_).


:- use_module(library(lists), [append/3]).

:- pred name_arity_to_atom(F/A,Pred_Atom): predname * var
	#"Instantiates @var{Pred_Atom} as an atom whose name
	  is the concatenation @var{F} + @var{'/'} + @var{A}.
          The instantiated atom is what we use as the canonical
	  predicate identifier.  This representation was chosen
	  to agree with the concept of a @concept{transactional predicate}.".
name_arity_to_atom(F/Ar, At) :-
	name(F,FS),
	append(FS,"/",Pre1),
	number_codes(Ar,AS),
	append(Pre1,AS,PredS),
	name(At,PredS).

%%if read-locked by only me: transform into write
%%if i have write lock - fall through
%%if locked in any other way block and try again when woken
%%if unlocked: take lock and return success
:- pred write_lock(F/A,Txn_ID) : atom * nnegint
	#"Attempts to obtain write lock for @var{F/A}.  The successful
	  invocation will complete immediately if currently unlocked 
	  or read/write locked only by @var{Txn_ID} or block if locked
	  in any other fashion.  Predicate will fail if blocking would
	  cause deadlock.".
write_lock(F/A, Txn_ID) :-
	lock_atom(critical_region),
        lock_table(F/A, "read", [Txn_ID]), !,
	retract_fact( lock_table(F/A, "read", [Txn_ID]) ),
	asserta_fact( lock_table(F/A, "write", Txn_ID) ),
	unlock_atom(critical_region).
write_lock(F/A, Txn_ID) :-
	lock_table(F/A, "write", Txn_ID), !,
	unlock_atom(critical_region).
write_lock(F/A, Txn_ID) :-
	lock_table(F/A, _, Owner_List), !,
	%display_string("ensure_no_deadlock("),
	%display(Owner_List), display_string(","),
	%display(Pred_Atom), display_string(","),
	%display(Txn_ID), display_string(")"), nl,
	(ensure_no_deadlock(Owner_List, Txn_ID, F/A) ->
	 unlock_atom(critical_region),
	 ensure_block(F/A),
	 write_lock(F/A, Txn_ID)
	;unlock_atom(critical_region),
	 fail).
write_lock(F/A, Txn_ID) :-
	asserta_fact( lock_table(F/A, "write", Txn_ID) ),
	unlock_atom(critical_region).

%%if read-locked: take lock and return success
%%if write-locked: block and try again when woken
%%if unlocked: take lock and return success
:- pred read_lock(F/A,Txn_ID) : atom * nnegint
	#"Attempts to obtain read lock for @var{F/A}.  The successful
	  invocation will complete immediately if currently unlocked 
	  or read-locked or block if write-locked.  Predicate will 
	  fail if blocking would cause deadlock.".
read_lock(F/A, Txn_ID) :-
	lock_atom(critical_region),
	lock_table(F/A, "read", Owner_List), !,
	retract_fact( lock_table(F/A, "read", Owner_List) ),
	%if i already have read lock should i
	%push another reference to me on list?
	%no
	(contains_ro(Owner_List, Txn_ID) ->
	 true 
	;asserta_fact( lock_table(F/A, "read", [Txn_ID | Owner_List]))),
	 unlock_atom(critical_region).
read_lock(F/A, Txn_ID) :-
	lock_table(F/A, "write", Owner), !,
	%display_string("ensure_no_deadlock("),
	%display(Owner), display_string(","),
	%display(Pred_Atom), display_string(","),
	%display(Txn_ID), display_string(")"), nl,
	(ensure_no_deadlock([Owner], Txn_ID, F/A) ->
	 unlock_atom(critical_region),
	 ensure_block(F/A),
	 read_lock(F/A, Txn_ID)
	;unlock_atom(critical_region),
	 fail).
read_lock(F/A, Txn_ID) :-
	asserta_fact( lock_table(F/A, "read", [Txn_ID]) ),
	unlock_atom(critical_region).


:- use_module(library(lists), [contains_ro/2]).

:- pred unlock(Txn_ID) : nnegint
	#"Attempts to release all locks assigned to @var{Txn_ID}.".
unlock(Txn_ID) :-
	lock_table(F/A, _, Txn_ID),
	un_lock(F/A, Txn_ID),
	fail.
unlock(Txn_ID) :-
	lock_table(F/A, _, OwnerList),
	contains_ro(OwnerList, Txn_ID),
	un_lock(F/A, Txn_ID),
	fail.
unlock(_).

:- pred un_lock(F/A, Txn_ID) : atom * nnegint
	#"Attempts to release appropriate lock.".
un_lock(F/A, Txn_ID) :-
	lock_atom(critical_region),
	lock_table(F/A, "write", Txn_ID), !,
	retract_fact( lock_table(F/A, "write", Txn_ID) ),
	cleanup_waitfor(F/A, Txn_ID),
	ensure_unblock(F/A),
	unlock_atom(critical_region).
un_lock(F/A, Txn_ID) :-
	lock_table(F/A, "read", [Txn_ID]), !,
	retract_fact( lock_table(F/A, "read", [Txn_ID]) ),
	cleanup_waitfor(F/A, Txn_ID),
	ensure_unblock(F/A),
	unlock_atom(critical_region).
un_lock(F/A, Txn_ID) :-
	lock_table(F/A, "read", [Owner1, Txn_ID]), !,
	retract_fact( lock_table(F/A, "read", [Owner1, Txn_ID]) ),
	asserta_fact( lock_table(F/A, "read", [Owner1]) ),
	cleanup_waitfor(F/A, Txn_ID),
	ensure_unblock(F/A),
	unlock_atom(critical_region).
un_lock(F/A, Txn_ID) :-
	lock_table(F/A, "read", [Txn_ID, Owner2]), !,
	retract_fact( lock_table(F/A, "read", [Txn_ID, Owner2]) ),
	asserta_fact( lock_table(F/A, "read", [Owner2]) ),
	cleanup_waitfor(F/A, Txn_ID),
	ensure_unblock(F/A),
	unlock_atom(critical_region).
un_lock(F/A, Txn_ID) :-
	lock_table(F/A, "read", Owner_List), !,
	delete(Owner_List, Txn_ID, New_Owner_List),
	retract_fact( lock_table(F/A, "read", Owner_List) ),
	asserta_fact( lock_table(F/A, "read", New_Owner_List) ),
	cleanup_waitfor(F/A, Txn_ID),
	unlock_atom(critical_region).
un_lock(_,_) :-
	unlock_atom(critical_region),
	fail.
