:- module(dead_code_elimination,[dead_code_elimination/1]).

:- use_package( .(ecce_no_rt) ).


/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,1996,1997 */
/* --------------------------------------------- */

/* TO GET THE LATEST VERSION OF THE ECCE SYSTEM GO TO:
	http://www.cs.kuleuven.ac.be/~michael */
/* BUG REPORTS, QUESTIONS AND COMMENTS CAN BE SENT TO:
	michael@cs.kuleuven.ac.be */

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(bimtools).
:- use_module(code_generator).
:- use_module(calc_chtree).

:- use_module(dynpreds).

%:- dynamic spec_clause/3.


:- include( multi_meta ).


/* file: dead_code_elimination.pro */

dead_code_elimination(IGoal) :- 
	reset_reachable,
	reset_var_call_encountered,
	l_add_reachable_atom(IGoal),
	dce_analysis,
	calc_dce_program.

l_add_reachable_atom(X) :- var(X),!.
l_add_reachable_atom([]).
l_add_reachable_atom([H|T]) :-
	add_reachable_atom(H),
	l_add_reachable_atom(T).

add_reachable_atom(Atom) :-
	is_calln(Atom), %Atom = call(_),
	!,
	(var_call_encountered -> true
	                      ;  print('CALL encountered'),nl,
	                         assert(var_call_encountered)).
add_reachable_atom(Atom) :-
	nonvar(Atom),
	get_predicate(Atom,pred(Pred,Arity)),
	\+(reachable(Pred,Arity)),!,
	assert(reachable(Pred,Arity)),
	debug_println(reachable(Pred,Arity)),
	(reachable_changed -> true ; assert(reachable_changed)).
add_reachable_atom(_).


calc_dce_program :-
	reset_spec_prog,
	claus(_Nr,Head,Body),
	get_predicate(Head,pred(Pred,Arity)),
	((reachable(Pred,Arity) ; var_call_encountered)
        -> (gensym(spec,SpecClauseNr),
	    assertz(spec_clause(SpecClauseNr,Head,Body))
           )
        ;  (cg_filter_goal(NodeID,_,_,Head) ->
             (retract(cg_filter_goal(NodeID,_,_,Head)),
              print('*'),
              debug_println(rem(NodeID)) )
            ; true)
        ),
	fail.
calc_dce_program :- verbose_nl.


dce_analysis :-
	verbose_print('.'),
	reset_reachable_changed,
	propagate_reachable,
	(reachable_changed -> dce_analysis ; verbose_nl).
	

propagate_reachable :-
	claus(_Nr,Head,Body),
	get_predicate(Head,pred(Pred,Arity)),
	reachable(Pred,Arity),
	member(Literal,Body),
	cg_extract_positive_atom_from_literal(Literal,Atom,_Struct,_Ptr),
	add_reachable_atom(Atom),
	fail.
propagate_reachable.
