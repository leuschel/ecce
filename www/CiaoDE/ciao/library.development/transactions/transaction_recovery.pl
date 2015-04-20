:- module(transaction_recovery, 
          [rollback/1], 
          [assertions,regtypes]).
:- comment(title,"Transaction Recovery").
:- comment(author,"Nick D. Pattengale, J.A. Navas").
:- comment(usage,"This module was written for internal use by the
	   @lib{transactions} module.  Therefore, unless you know
	   what you are doing you should not plan to use this module
	   directly in user code.").

:- use_module(transaction_logging).
:- use_module(transaction_concurrency).
:- use_module(library(system), [time/1]).

%--------------------------------------------------------------%
:-pred rollback(Txn_ID): transaction
	#"Undo all operations carried out by @var{Txn_ID}.".
%--------------------------------------------------------------%

rollback(Txn_ID) :-
	time(T),
        display_string("Txn "),display(Txn_ID),
        display_string(" starts rollback ...."),nl,
	asserta_fact(dbmsj(T, Txn_ID, rollback)),
	fail.
rollback(Txn_ID) :-
	time(T),
	dbmsj(T, Txn_ID, Action),
	( (Action = asserta_fact(Fact)) ; (Action = assertz_fact(Fact)) ->
          display_string("Undoing an assert..."),nl,
	  time(T1),
	  asserta_fact(dbmsj(T1, Txn_ID,retract_fact(Fact))),
	  retract_fact(Fact)
	; 
          (Action = retract_fact(Fact) ->
           display_string("Undoing a retract..."),nl,
	   time(T2),
	   asserta_fact(dbmsj(T2, Txn_ID,assertz_fact(Fact))),
	   assertz_fact(Fact)
          ;
           true
          )
        ),
	fail.
rollback(Txn_ID):-
	display_string("Txn "),display(Txn_ID),
        display_string(" finishes the rollback"),nl,
	unlock(Txn_ID).



%% We have to ensure rollback/1 is idempotence
%% It has to read the operations in the reverse order !!
