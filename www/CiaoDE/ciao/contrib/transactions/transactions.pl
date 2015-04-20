%%transactions.pl
%%(c)2003 Nick D. Pattengale
%% nickp@cs.unm.edu

:- set_prolog_flag(multi_arity_warnings, off).

:- load_compilation_module(transactionstr).
:- add_sentence_trans(data_decl_expander_tr/2).
:- add_term_trans(data_goal_expander_tr/4).
:- add_term_trans(asserta_expander_tr/4).
:- add_term_trans(assertz_expander_tr/4).
:- add_term_trans(retract_expander_tr/4).
%%:- add_term_trans(retractall_expander_tr/4).
:- add_term_trans(txn_decl_expander_tr/2).
:- add_sentence_trans(transaction_expander_tr/3).

:- use_module(transactionsrt).
:- use_module(transaction_logging).
:- use_module(transaction_concurrency).
:- use_module(library(system), [time/1]).
rollback(Txn_ID) :-
	time(T),
	asserta_fact(dbmsj(T, Txn_ID, rollback)),
	fail.
rollback(Txn_ID) :-
	time(T),
	dbmsj(T, Txn_ID, Next),
	(Next = asserta_fact(Fact) ->
	 time(T1),
	 asserta_fact(dbmsj(T1, Txn_ID,retract_fact(Fact))),
	 retract_fact(Fact)
	;(Next = assertz_fact(Fact) ->
	  time(T2),
	  asserta_fact(dbmsj(T2, Txn_ID,retract_fact(Fact))),
	  retract_fact(Fact)
	 ;(Next = retract_fact(Fact) ->
	   time(T3),
	   asserta_fact(dbmsj(T3, Txn_ID,assertz_fact(Fact))),
	   assertz_fact(Fact)
	  ;true))),
	   fail.
rollback(_).
