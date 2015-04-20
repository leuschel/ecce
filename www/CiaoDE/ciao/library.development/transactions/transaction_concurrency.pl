:- module(transaction_concurrency,
          [write_lock/2,read_lock/2,unlock/1],
          [assertions,regtypes]).

:- comment(title,"Shared Read/Write Locks").
:- comment(subtitle, "Providing Support for Strict Two-Phase 
	              Locking with Deadlock Avoidance").
:- comment(author,"Nick D. Pattengale, J.A. Navas").
:- comment(usage,"This module was written for internal use by the
	   @lib{transactions} module.  Therefore, unless you know
	   what you are doing you should not plan to use this module
	   directly in user code.").

:- use_module(library(lists), [contains_ro/2,delete/3,append/3]).
:- use_module(library(concurrency), 
              [atom_lock_state/2,lock_atom/1,unlock_atom/1]).
:- use_module(library(write)).
:- use_module(transaction_logging,[transaction/1]).

:- comment(doinclude,lock_table/3).
:- comment(doinclude,waitfor_arc/3).
:- comment(doinclude,lock/1).

:-pred lock_table(Item, Lock, Txns): predname * lock * list(transaction)
	#"Represents a table in which each entry indicates that the
         transactions @var{Txns} that hold a lock on @var{Item}.".
:- data(lock_table/3).

:- pred waitfor_arc(T1,T2,X): transaction * transaction * predname
	#"Represents a wait-for graph in which each node indicates that the
	  transaction @var{T2} is waiting to lock an item @var{X} that is
	  currently locked by a transaction @var{T1}. This data structure
	  detects states of deadlock since the existence of a cycle in the
	  graph means deadlock. A cycle is T1 -> T2 -> ... -> Tn -> T1".
:- data(waitfor_arc/3).

reachable(V,V).
reachable(V1,V2):-
	waitfor_arc(V1,V,_),
	reachable(V,V2).

%% There is a cycle in a WFG when T1 -> T2 -> ... -> Tn -> T1
no_deadlock([],_,_).
no_deadlock([T|Txs],T,F/A) :- 
 	no_deadlock(Txs,T,F/A).
no_deadlock([Tx|_],T,_) :-      % there exists a cycle
	reachable(Tx,T),!,
	fail.  
no_deadlock([Tx|Txs],T,F/A):-
 	( waitfor_arc(T,Tx,F/A) -> 
	  true
 	;
 	  asserta_fact(waitfor_arc(T,Tx,F/A))
        ),
        no_deadlock(Txs,T,F/A).

cleanup_waitfor(F/A,Txn_ID):-
	retract_fact( waitfor_arc(_,Txn_ID,F/A) ),
	fail.
cleanup_waitfor(F/A,Txn_ID):-
	retract_fact( waitfor_arc(Txn_ID,_,F/A) ),
	fail.
cleanup_waitfor(_,_).

%only for debuging
print_waitfor_arc :-
	waitfor_arc(Vi,Vj,Arc),
	display(Vi),display_string(" --> "),display(Vj), display_string(" ["), 
	display(Arc),display_string("]"),nl,
	fail.
print_waitfor_arc:- nl,!.

:- pred wait(Item): predname
        #"The current thread waits until a nonzero value is reached in the
         semaphore associated to @var{Item}.".
wait(F/A) :-
	name_arity_to_atom(F/A, Pred_Atom),
	atom_lock_state(Pred_Atom, Value),
	Value = 1,
	lock_atom(Pred_Atom),
	lock_atom(Pred_Atom).
wait(F/A) :-
	name_arity_to_atom(F/A, Pred_Atom),
	atom_lock_state(Pred_Atom, Value),
	Value = 0,
	lock_atom(Pred_Atom).

:- pred wakeup(Item): predname

        #"The semaphore associated with @var{Item} is incremented if it is
         zero, waking up one of the waiting transactions, if any.".

wakeup(F/A) :-
	name_arity_to_atom(F/A, Pred_Atom),
	atom_lock_state(Pred_Atom, Value),
	Value = 0,
	unlock_atom(Pred_Atom).
wakeup(_).

%----------------------------------------------------------------------%
:- pred write_lock(Pred,Txn_ID) : predname * transaction
	#"Attempts to obtain write lock for @var{Pred}.  The successful
	  invocation will complete immediately if currently unlocked 
	  or read/write locked only by @var{Txn_ID} or block if locked
	  in any other fashion.  Predicate will fail if blocking would
	  cause deadlock.".
%----------------------------------------------------------------------%
%% LOCK(X) = read_lock  (upgrading)
write_lock(F/A, Txn_ID) :-
	lock_atom(critical_region),
        lock_table(F/A, "read", [Txn_ID]), !,
	retract_fact( lock_table(F/A, "read", [Txn_ID]) ),
	asserta_fact( lock_table(F/A, "write", [Txn_ID]) ),
        display_string("Txn "), write(Txn_ID), 
        display_string(" acquires a  write lock (upgrading) on "), 
        write(F/A),nl,
	unlock_atom(critical_region).

%% LOCK(X) = write_lock (keeps the write-lock)
write_lock(F/A, Txn_ID) :-
	lock_table(F/A, "write", [Txn_ID]), !,
        %display_string("Txn "),write(Txn_ID),
        %display_string(" keeps  a write lock on "),
        %write(F/A),nl,      
	unlock_atom(critical_region).


%% LOCK(X) \== unlocked  (more than one readers or different owner)
write_lock(F/A, Txn_ID) :-
	lock_table(F/A,_,Owner_List), !,
	( no_deadlock(Owner_List, Txn_ID, F/A) ->
          display_string("Txn "),write(Txn_ID),
          display_string(" cannot get a write-lock on "),write(F/A),
          display_string(" and suspends"),nl,
	  unlock_atom(critical_region),
	  wait(F/A),                 %% the process is blocked
          display_string("Txn "),write(Txn_ID),
          display_string(" awaken"),nl,
	  write_lock(F/A, Txn_ID)    %% when it has been awaken, tries again
	;
          display_string("Possible deadlock between "),display(Txn_ID), 
	  display_string(" and one of the transactions: "),
          display(Owner_List),nl,
	  print_waitfor_arc,
          unlock_atom(critical_region),
	  fail
        ).

%% LOCK(X) = unlocked
write_lock(F/A, Txn_ID) :-
	asserta_fact( lock_table(F/A, "write", [Txn_ID]) ),
        display_string("Txn "),write(Txn_ID),
        display_string(" gets a write-lock on "),
        write(F/A),nl,
	unlock_atom(critical_region).

%----------------------------------------------------------------------%
:- pred read_lock(Pred,Txn_ID) : predname * transaction
	#"Attempts to obtain read lock for @var{Pred}.  The successful
	  invocation will complete immediately if currently unlocked 
	  or read-locked or block if write-locked.  Predicate will 
	  fail if blocking would cause deadlock.".
%----------------------------------------------------------------------%
% LOCK(X) = read_lock
read_lock(F/A, Txn_ID) :-
	lock_atom(critical_region),
	lock_table(F/A, "read", Owner_List), !,
	retract_fact( lock_table(F/A, "read", Owner_List) ),
	(contains_ro(Owner_List, Txn_ID) ->
         display_string("Txn "),write(Txn_ID),
         display_string(" already hold a read-lock on "),write(F/A),nl,
	 true 
	;
         display_string("Txn "),write(Txn_ID),
         display_string(" gets a read-lock on "),write(F/A),nl,
         asserta_fact( lock_table(F/A, "read", [Txn_ID | Owner_List]))),
	 unlock_atom(critical_region
        ).

% LOCK(X) = write_lock    (downgrading)
read_lock(F/A, Txn_ID) :-
	lock_table(F/A, "write", [Txn_ID]), !,
        retract_fact( lock_table(F/A,"write",[Txn_ID]) ),
        asserta_fact( lock_table(F/A,"read",[Txn_ID]) ),
        display_string("Txn "), write(Txn_ID), 
        display_string(" acquires a  read lock (downgrading) on "), 
        write(F/A),
        unlock_atom(critical_region).

% LOCK(X) = write_lock
read_lock(F/A, Txn_ID) :-
	lock_table(F/A, "write", [Owner]), !,
	( no_deadlock([Owner], Txn_ID, F/A) ->
          display_string("Txn "),write(Txn_ID),
          display_string(" cannot get a read-lock on "),write(F/A),
          display_string(" and suspends"),nl,
	  unlock_atom(critical_region),
	  wait(F/A),              %% the process is blocked
          display_string("Txn "),write(Txn_ID),
          display_string(" awaken"),nl,
	  read_lock(F/A, Txn_ID)  %% when it has been awaken, tries again 
	;
          display_string("Possible deadlock between "), display(Txn_ID), 
	  display_string(" and "), display(Owner),nl,
	  print_waitfor_arc,
          unlock_atom(critical_region),
	  fail
        ).

% LOCK(X) = unlocked
read_lock(F/A, Txn_ID) :-
	asserta_fact( lock_table(F/A, "read", [Txn_ID]) ),
        display_string("Txn "),write(Txn_ID),
        display_string(" gets a read-lock on "),write(F/A),nl,
	unlock_atom(critical_region).

%----------------------------------------------------------------------%
:- pred unlock(Txn_ID) : transaction
	#"Attempts to release all locks assigned to @var{Txn_ID}.".
%----------------------------------------------------------------------%
unlock(Txn_ID) :-
	lock_table(F/A,_,[Txn_ID]),
	un_lock(F/A, Txn_ID),
	fail.
unlock(Txn_ID) :-
	lock_table(F/A,_,OwnerList),
	contains_ro(OwnerList, Txn_ID),
	un_lock(F/A, Txn_ID),
	fail.
unlock(_).

%----------------------------------------------------------------------%
:- pred un_lock(Pred, Txn_ID) : predname * transaction
	#"Attempts to release appropriate lock to @var{Txn_Id} on @var{Pred}. ".
%----------------------------------------------------------------------%
% Txn_ID releases a write-lock on F/A
un_lock(F/A, Txn_ID) :-
	lock_atom(critical_region),
	lock_table(F/A, "write", [Txn_ID]), !,
	retract_fact( lock_table(F/A, "write", [Txn_ID]) ),
	cleanup_waitfor(F/A, Txn_ID),
        display_string("Txn "),write(Txn_ID),
        display_string(" releases a write-lock on "),write(F/A),
        display_string(" and wakeup on of the waiting transactions, if any"), 
        nl,    %%%%%%%%%%%%%%%%%
   	wakeup(F/A),
	unlock_atom(critical_region).

% Txn_ID hold a read-lock on F/A and no_of_reads(F/A) == 0
un_lock(F/A, Txn_ID) :-
	lock_table(F/A, "read", [Txn_ID]), !,
	retract_fact( lock_table(F/A, "read", [Txn_ID]) ),
	cleanup_waitfor(F/A, Txn_ID),
        display_string("Txn "),write(Txn_ID),
        display_string(" releases a read-lock on "),write(F/A),
        display_string(" and wakeup on of the waiting transactions, if any"),
        nl,
	wakeup(F/A),
	unlock_atom(critical_region).

% Txn_ID hold a read-lock on F/A and no_of_reads(F/A) \== 0
% un_lock(F/A, Txn_ID) :-
% 	lock_table(F/A, "read", [Owner1, Txn_ID]), !,
% 	retract_fact( lock_table(F/A, "read", [Owner1, Txn_ID]) ),
% 	asserta_fact( lock_table(F/A, "read", [Owner1]) ),
% 	cleanup_waitfor(F/A, Txn_ID),
%         display_string("Txn "),write(Txn_ID),
%         display_string(" releases a read-lock on "),write(F/A),nl,
% 	wakeup(F/A),
% 	unlock_atom(critical_region).
% un_lock(F/A, Txn_ID) :-
% 	lock_table(F/A, "read", [Txn_ID, Owner2]), !,
% 	retract_fact( lock_table(F/A, "read", [Txn_ID, Owner2]) ),
% 	asserta_fact( lock_table(F/A, "read", [Owner2]) ),
% 	cleanup_waitfor(F/A, Txn_ID),
%         display_string("Txn "),write(Txn_ID),
%         display_string(" releases a read-lock on "),write(F/A),nl,
% 	wakeup(F/A),
% 	unlock_atom(critical_region).
% un_lock(F/A, Txn_ID) :-
% 	lock_table(F/A, "read", Owner_List), !,
% 	delete(Owner_List, Txn_ID, New_Owner_List),
% 	retract_fact( lock_table(F/A, "read", Owner_List) ),
% 	asserta_fact( lock_table(F/A, "read", New_Owner_List) ),
% 	cleanup_waitfor(F/A, Txn_ID),
%         display_string("Txn "),write(Txn_ID),
%         display_string(" releases a read-lock on "),write(F/A),nl,
% 	unlock_atom(critical_region).

un_lock(F/A, Txn_ID) :-
	lock_table(F/A, "read", Owner_List), !,
	delete(Owner_List, Txn_ID, New_Owner_List),
	retract_fact( lock_table(F/A, "read", Owner_List) ),
	asserta_fact( lock_table(F/A, "read", New_Owner_List) ),
	cleanup_waitfor(F/A, Txn_ID),
        display_string("Txn "),write(Txn_ID),
        display_string(" releases a read-lock on "),write(F/A),nl,
        wakeup(F/A),	
        unlock_atom(critical_region).

un_lock(_,_) :-
	unlock_atom(critical_region),
	fail.

% :- pred name_arity_to_atom(F/A,Pred_Atom): predname * atom
% 	#"Instantiates @var{Pred_Atom} as an atom whose name
% 	  is the concatenation @var{F} + @var{'/'} + @var{A}.
%           The instantiated atom is what we use as the canonical
% 	  predicate identifier.  This representation was chosen
% 	  to agree with the concept of a @concept{transactional predicate}.".
name_arity_to_atom(F/Ar, At) :-
	name(F,FS),
	append(FS,"/",Pre1),
	number_codes(Ar,AS),
	append(Pre1,AS,PredS),
	name(At,PredS).
%----------------------------------------------------------------------%
% Regtypes                                                             |
%----------------------------------------------------------------------%
:- regtype lock(L) #"@var{L} is a lock.".

lock(read).
lock(write).
