%%transaction_logging.pl
%%(c)2003 Nick D. Pattengale
%% nickp@cs.unm.edu

:- module(transaction_logging, [start_transaction/1,abort/1,commit/1,read/2,write/2,dbmsj/3], [assertions,regtypes]).

:- use_module(transaction_concurrency, [unlock/1]).

:- comment(title,"Transaction Logging").

:- comment(copyright, "@copyright 2003 Nick D. Pattengale").

:- comment(author,"Nick D. Pattengale - @email{nickp@@cs.unm.edu}").

:- comment(usage,"This module was written for internal use by the
	   @lib{transactions} module.  Therefore, unless you know
	   what you are doing you should not plan to use this module
	   directly in user code.").

:- data(transaction_id/1).

:-pred dbmsj(Timestamp, Txn_ID, Action): nnegint * nnegint * callable
	#"Referred to in database literature as the DBMS Journal,
	this table is a record of database accesses (reads/writes).
        @var{Timestamp} is when the action occured. @var{Txn_ID}
	is the transaction identifier of the transaction that made
	the database access.  And, @var{Action} is the action taken
	by the transaction.  The DBMSJ is useful (and essential) 
	for rollbacks".
:- data(dbmsj/3).

:- use_module(library(system), [time/1]).

transaction_id(0).
:- pred start_transaction(Txn_ID): var
	#"Instantiates @var{Txn_ID} to a fresh non negative integer
	  that can be used to uniquely identify a transaction.".
start_transaction(Txn_ID) :-
	transaction_id(Last_ID),
	retract_fact( transaction_id(Last_ID) ),
	Txn_ID is Last_ID + 1,
	set_fact( transaction_id(Txn_ID) ),
	time(T),
	asserta_fact( dbmsj(T, Txn_ID, start_transaction) ).
	%%display_string("DBMSJ: start_transaction["),
	%%display(Txn_ID),
	%%display_string("]"), nl.

 
:- use_module(library(lists), [contains_ro/2]).

:- pred abort(Txn_ID) : nnegint
	#"Rolls back side effects of @var{Txn_ID}, 
	  releases all locks associated with @var{Txn_ID} and
	  records the abort in the transaction log.".
abort(Txn_ID) :- 
	unlock(Txn_ID),
	time(T),
	asserta_fact( dbmsj(T, Txn_ID, abort) ).
	%display_string("DBMSJ: abort["), 
	%display(Txn_ID),
	%display_string("]"), nl.

:- pred commit(Txn_ID) : nnegint
	#"Releases all locks associated with @var{Txn_ID} and
	  records the commit in the transaction log.".
commit(Txn_ID) :- 
	unlock(Txn_ID),
	time(T),
	asserta_fact( dbmsj(T, Txn_ID, commit) ).
	%%display_string("DBMSJ: commit["), 
	%%display(Txn_ID), 
	%%display_string("]"), nl.


:- pred read(Txn_ID,Fact): nnegint * callable
	#"Records @var{Fact} in the DBMSJ.".
read(Txn_ID, Fact) :-
	time(T),
	asserta_fact( dbmsj(T, Txn_ID, Fact) ).
	%%display_string("DBMSJ: read["),
	%%display(Txn_ID),
	%%display_string("]("),
	%%display(Fact),
	%%display_string(")"), nl.

:- pred write(Txn_ID,Fact): nnegint * callable
	#"Records @var{Fact} in the DBMSJ.".
write(Txn_ID, Fact) :-
	time(T),
	asserta_fact( dbmsj(T, Txn_ID, Fact) ).
	%%display_string("DBMSJ: write["),
	%%display(Txn_ID),
	%%display_string("]("),
	%%display(Fact),
	%%display_string(")"), nl.
