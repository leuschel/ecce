:- module(msv_analysis,
	[
	    msv_change/0, 
	    calc_and_store_msv_result/0,
	    msv_of_goal/1,
	    run_msv_anlysis/0,
	    calc_msv_clauses/0,
	    assert_msv_change/0,
	    retract_msv_change/0,
	    bup/0,
	    propagation_step/0,
	    take_msg/2,
	    propagate_tuple/1,
	    unify_body/1,
	    is_a_fact/1,
	    same_predicate/2
        ]).

/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,1996,1997 */
/* --------------------------------------------- */

/* TO GET THE LATEST VERSION OF THE ECCE SYSTEM GO TO:
	http://www.cs.kuleuven.ac.be/~michael */
/* BUG REPORTS, QUESTIONS AND COMMENTS CAN BE SENT TO:
	michael@cs.kuleuven.ac.be */

:- set_prolog_flag(multi_arity_warnings,off).
%:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(bimtools).
:- use_module(code_generator).
:- use_module(static_dynamic_functors).
:- use_module(calc_chtree).

:- use_module(dynpreds).

:- use_package( .(ecce_no_rt) ).
%not(Goal) :- \+(Goal).


/* file: msv_analysis.pro */


:- dynamic msv_change/0.
msv_change.

:- dynamic msv_entry/1.
msv_entry([]).


calc_and_store_msv_result :-
	retract(msv_entry(_X)),fail.
calc_and_store_msv_result :-
	bup.

msv_of_goal(Goal) :-
	unify_body(Goal).



run_msv_anlysis :-
	reset_spec_prog,
	reset_static_functors,
	verbose_println('-> calculating static functors'),
	calculate_static_functors,
	verbose_println('-> starting bup propagation'),
	bup,verbose_nl,
	retractall(msv_change),
	calc_msv_clauses.

calc_msv_clauses :-
	claus(_Nr,Head,Body),
	copy(clause(Head,Body),CopyOfClause),
	((unify_body(Body),
	  unify_body([Head])
	 )
	 -> (assert_spec_clause(Head,Body),
	     (variant_of(clause(Head,Body),CopyOfClause)
		-> true
		;   assert_msv_change,
		    debug_println(change(clause(Head,Body),CopyOfClause))
	    ))
	 ;  assert_msv_change,
	    debug_println(change(clause(Head,[fail]))),
	    assert_unsimplified_spec_clause(Head,[fail])
	),
	fail.
calc_msv_clauses.


assert_msv_change :-
	(msv_change -> true ; assert(msv_change)).

retract_msv_change :- retract(msv_change),fail.
retract_msv_change.

bup :-
	debug_println(bup),
	retract_msv_change,
	propagation_step,
	(msv_change
	   -> verbose_print('.'),bup
	   ;  true
	).

propagation_step :-
	defined_predicate(P,Arity),
	functor(G,P,Arity),
	findall(G,propagate_tuple(G),NewGs), 
	take_msg(NewGs,NewMsg),
	(retract(msv_entry(G)) -> OldG=G ; OldG=[fail]),
	assert(msv_entry(NewMsg)),
	(variant_of(OldG,NewMsg)-> true ;
	  assert_msv_change,debug_println(msv_chg(OldG,NewMsg))),
	fail.
propagation_step.


take_msg([H|T],Res) :-
    compute_msg(H,H,MsgH), /* for simple_msv */
	takemsg2(T,MsgH,Res).

takemsg2([],R,R).
takemsg2([H|T],A,R) :-
	compute_msg(H,A,MsgHA),!,
	takemsg2(T,MsgHA,R).


:- dynamic simple_msv/1.
simple_msv(no). /* if yes: just propagate failure bottom-up */

compute_msg(A,B,Msg) :- simple_msv(yes),!,
 /* just copy predicate info */
  functor(A,F,Arity),
  functor(B,F,Arity),
  functor(Msg,F,Arity).
compute_msg(A,B,Msg) :- msg(A,B,Msg).




propagate_tuple(Head) :-
	claus(_Nr,Head,Body),
	unify_body(Body),
	debug_print(prop(Head)).


% unify body with msv answers found so far
unify_body([]).
unify_body([H|T]) :- unify_body_atom(H), unify_body(T).

unify_body_atom(not(B1)) :- !,
	\+(is_a_fact(B1)). /* then assume negation succeeds */
unify_body_atom(\+(B1)) :- !, 
	\+(is_a_fact(B1)). /* then assume negation succeeds */
unify_body_atom(BI) :-
	is_built_in_literal(BI),!,
	( is_callable_built_in_literal(BI) -> call_built_in(BI)
	 ; true /* suppose other built-ins succeed */
	).
unify_body_atom(B1) :- msv_entry(B1),!.

is_a_fact(B) :-
	copy(B,BC),
	claus(_Nr,BC,[]),
	variant_of(B,BC).


same_predicate(X,Y) :-
	X =.. [P|ArgsX],
	Y =.. [P|ArgsY],
	same_length(ArgsX,ArgsY).

/* already defined in Sicstus:
same_length([],[]).
same_length([_|XT],[_|YT]) :- same_length(XT,YT).
*/

