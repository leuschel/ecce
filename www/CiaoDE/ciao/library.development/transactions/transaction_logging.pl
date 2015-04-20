:- module(transaction_logging,
         [start_transaction/1,abort/1,commit/1,read/2,write/2,dbmsj/3,
          transaction/1],
         [assertions,regtypes,persdb]).
:- use_module(transaction_concurrency, [unlock/1]).
:- comment(title,"Transaction Logging").
:- comment(author,"Nick D. Pattengale, J.A. Navas").
:- comment(usage,"This module was written for internal use by the
	   @lib{transactions} module.  Therefore, unless you know
	   what you are doing you should not plan to use this module
	   directly in user code.").

:- use_module(library(system), [time/1]).
:- use_module(library(lists), [contains_ro/2]).
:- use_module(library(write)).

:- persistent(dbmsj/3,logging).
:- persistent(transaction_id/1,txn_counter).
persistent_dir(logging,'./log').
persistent_dir(txn_counter,'./log').

:- comment(nodoc,persdb).
:- comment(doinclude,dbmsj/3).

:-pred dbmsj(Timestamp, Txn_ID, Action): nnegint * transaction * callable
	#"Referred to in database literature as the DBMS Journal,
	this table is a record of database accesses (reads/writes).
        @var{Timestamp} is when the action occured. @var{Txn_ID}
	is the transaction identifier of the transaction that made
	the database access.  And, @var{Action} is the action taken
	by the transaction.  The DBMSJ is useful (and essential) 
	for rollbacks".

:- pred start_transaction(Txn_ID): transaction
	#"Instantiates @var{Txn_ID} to a fresh non negative integer
	  that can be used to uniquely identify a transaction.".
start_transaction(Txn_ID) :-
	( transaction_id(Last_ID) ->
   	  retract_fact( transaction_id(Last_ID) ),
 	  Txn_ID is Last_ID + 1,
	  asserta_fact( transaction_id(Txn_ID) )
        ; 
          asserta_fact( transaction_id(1) ),
          Txn_ID = 0
        ),
	time(T),
	asserta_fact( dbmsj(T, Txn_ID, start_transaction) ).

:- pred abort(Txn_ID) : transaction
	#"Rolls back side effects of @var{Txn_ID}, 
	  releases all locks associated with @var{Txn_ID} and
	  records the abort in the transaction log.".
abort(Txn_ID) :- 
%	unlock(Txn_ID),
        display_string("Txn "),write(Txn_ID),
        display_string(" aborts"),nl,
	time(T),
	asserta_fact( dbmsj(T, Txn_ID, abort) ).

:- pred commit(Txn_ID) : transaction
	#"Releases all locks associated with @var{Txn_ID} and
	  records the commit in the transaction log.".
commit(Txn_ID) :- 
	unlock(Txn_ID),
        display_string("Txn "),write(Txn_ID),
        display_string(" commits and releases all locks"),nl,
	time(T),
	asserta_fact( dbmsj(T, Txn_ID, commit) ).


:- pred read(Txn_ID,Fact): transaction * callable
	#"Records @var{Fact} in the DBMSJ.".
read(_,_) .


:- pred write(Txn_ID,Fact): transaction * callable
	#"Records @var{Fact} in the DBMSJ.".
write(Txn_ID, Fact) :-
	time(T),
	asserta_fact( dbmsj(T, Txn_ID, Fact) ).

:- comment(transaction/1,"@var{T} is a transaction.
  @begin{itemize}
  @item @em{Code:}
  @includedef{transaction/1}
  @end{itemize}
").

:- regtype transaction(Id) #"@var{Id} is a transaction.".

transaction(Id):- nnegint(Id).

:- comment(lock/1,"@var{L} is a lock.
  @begin{itemize}
  @item @em{Code:}
  @includedef{lock/1}
  @end{itemize}
").
