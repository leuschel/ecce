:- module(transactionstr, 
	[
	 data_decl_expander_tr/2,
	 data_goal_expander_tr/4,
	 asserta_expander_tr/4,
	 assertz_expander_tr/4,
	 retract_expander_tr/4,
	 abort_expander_tr/4,
         transaction_expander_tr/3,
	 transaction_decl_expander_tr/2
	], []). 
 

%here the compiler keeps a table of data_fact
%predicates so that other expansions can recognize
%them... 
:- data(data_fact/1).

data_decl_expander_tr(0,_) :- 
	retractall_fact(data_fact(_/_)),
	retractall_fact(iei(_,_)).
data_decl_expander_tr( (:- data(F/A)), (:- data(F/A)) ) :-
	assertz_fact( data_fact(F/A) ).


%use following data_facts to hack our way
%around infinite expansion.  For example, 
%since we want Fact to expand to Fact, read(Txn_ID, Fact)
%we use facts "one" and "two" to keep state w.r.t.
%how many times Fact has been expanded so far.
:- data(iei/2).

update_iei(OpKind) :-
	(iei(OpKind,1) ->
	 retract_fact(iei(OpKind,1)),
	 assertz_fact(iei(OpKind,2)),
	 fail
	;(iei(OpKind,2) ->
	  retract_fact(iei(OpKind,2)),
	  fail
	 ;(assertz_fact(iei(OpKind,1))))).

read_inhibit :-
	assertz_fact( iei(read,1) ).

:- data(in_transaction_body/0).

data_goal_expander_tr( Fact, 
	(( read_lock( F/A, Txn_ID ) -> true
         ; !,fail),
	 Fact,
	 read(Txn_ID, Fact)),
	  _Module, [Txn_ID]) :-
 in_transaction_body,
 functor(Fact,F,A),
 data_fact(F/A),
 update_iei(read).

%the full set of mutators for data_facts is
%------
%asserta_fact/1/2, assertz_fact/1/2, current_fact/1/2
%retract_fact/1, retractall_fact/1, current_fact_nb/1/2
%retract_fact_nb/1, set_fact/1, erase/1
%------
%however persdb only provides 
%asserta_fact/assertz_fact/retract_fact/retractall_fact
%we will follow suit and only provide the same subset...
%a truly motivated programmer could add the others if needed

asserta_expander_tr( asserta_fact( Fact ), 
	             ( ( write_lock( F/A, Txn_ID ) -> 
			 true
                       ; 
			 !,fail
                       ),
		       asserta_fact(Fact),
		       write(Txn_ID, asserta_fact(Fact))         
		     ),
		   _Module, [Txn_ID] ) :-

 in_transaction_body,
 functor(Fact, F, A),
 data_fact(F/A),
 update_iei(asserta),
 read_inhibit.

assertz_expander_tr( assertz_fact( Fact ), 
	( ( write_lock( F/A, Txn_ID ) -> 
	    true
	  ; 
	    !, fail
	  ),  
	 assertz_fact(Fact),
	 write(Txn_ID, assertz_fact(Fact))
	),
	 _Module, [Txn_ID] ) :-

 in_transaction_body,
 functor(Fact, F, A),
 data_fact(F/A),
 update_iei(assertz),
 read_inhibit.

retract_expander_tr( retract_fact( Fact ),
	( ( write_lock( F/A, Txn_ID ) -> 
	    true
          ; 
	    !,fail
	  ),
	  retract_fact(Fact),
	  write(Txn_ID, retract_fact(Fact))
	),
	 _Module, [Txn_ID] ) :-
 in_transaction_body,
 functor(Fact, F, A),
 data_fact(F/A),
 update_iei(retract),
 read_inhibit.

%%can't easily do the inverse of a retractall
%retractall_expander_tr( retractall_fact( Fact ), 
%	(write_lock( F/A, Txn_ID ) ->
%	 !, retractall_fact(Fact), write(Txn_ID, retractall_fact(Fact))
%	;abort(Txn_ID), pause(1), Txn_Call, fail), 
%	 _Module, [Txn_ID, Txn_Call] ) :-
% functor(Fact, F, A),
% data_fact(F/A),
% assertz_fact(one).

:- use_module(library(terms)).

%compiler also keeps around a table of
%transaction definitions so that they
%can be expanded
:- data(transaction/1).
transaction_decl_expander_tr(0,_) :- retractall_fact(transaction(_/_)).
transaction_decl_expander_tr( (:- transactional(F/A)), 
	(TxnHelper :- start_transaction(X), 
	              ( Txn_Call ->
		        commit(X)
		      ;
		        abort(X),
			rollback(X),
			pause(1),
			TxnHelper
		      )
        ) ) :-

 A1 is A+1,
 functor(Txn_Call, F, A1),
 arg(A1, Txn_Call, X),
 functor(TxnHelper, F, A), 
 copy_args(A, Txn_Call, TxnHelper), 
 assertz_fact(transaction(F/A)).

abort_expander_tr(  abort,
	           (abort(Txn_ID),
		    rollback(Txn_ID)),
		    _Module, [Txn_ID]):-
 in_transaction_body.

:- use_module(library('compiler/translation'), [expand_term/4]).

transaction_expander_tr( (Head :- Body), (NewHead :- ExpandedBody), Module ) :-
	functor(Head, F, A),
	transaction(F/A),
	A1 is A+1,
	functor(NewHead, F, A1),
	copy_args(A, Head, NewHead),
	arg(A1, NewHead, X),
	%%manually invoke early translation of the body
        %%because we need X in the start_transaction to
        %%be the same private variable as the Txn_ID's in
        %%the expanded body...
        assertz_fact(in_transaction_body),
	expand_term(Body, Module, [X], ExpandedBody),
	retract_fact(in_transaction_body).
