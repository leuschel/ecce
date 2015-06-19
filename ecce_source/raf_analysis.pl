:- module(raf_analysis,
	[
	    perform_raf_analysis/0,
	    perform_raf_analysis/1,
	    add_atom_of_interest/1,
	    update_cg_filter_goal/0,
	    calc_erased_program/0,
	    raf_analysis/0,
	    propagate_erasure/0,
	    assert_dont_erase/3,
	    get_sub_term/5,
	    unsafe_erasure/3,
	    variable_occurs/3,
	    erase_literal/2,
	    erase_atom/2,
	    erase_arguments/5,
	    perform_andprint_far_analysis/0,
	    perform_far_analysis/1,
	    far_analysis/0,
	    propagate_far_erasure/0,
	    unsafe_far_erasure/3
	]).

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

:- use_module(library(lists)).

:- use_module(bimtools). 
:- use_module(code_generator).
:- use_module(static_dynamic_functors).
:- use_module(main_functions).

:- use_module(dynpreds).

:- include( multi_meta ).


/* file: raf-analysis.pro */



perform_raf_analysis :-
	print('Atom or goal of interest ==>'),
	read(IGoal),
	perform_raf_analysis(IGoal).

perform_raf_analysis(IGoal) :-
	recalc_static_functors,
	verbose_print(' --> performing raf analysis for '), verbose_print(IGoal),verbose_nl,
	(IGoal = [_|_]
	 -> (calculate_static_functors_for_query(IGoal),
	     perform_raf_analysis2(IGoal))
	 ;  (calculate_static_functors_for_query([IGoal]),
	     perform_raf_analysis2([IGoal]))
	),
	(var_call_encountered
	 -> (print('### Warning: call(_) encountered'),nl,
	     print('### Filtered program could be incorrect !'),nl,
	     print('### Keep the original program to be safe !'),nl
	    )
	 ;  true
	).
	%print_specialised_program_to_file('RAF Analysis Result').

perform_raf_analysis2(IGoal) :- 
	reset_dont_erase,
	reset_nr_of_erased_arguments,
	reset_var_call_encountered,
	l_add_atom_of_interest(IGoal),
	raf_analysis,
	reset_nr_of_erased_arguments,
	calc_erased_program,
	update_cg_filter_goal,
	nr_of_erased_arguments(Nr),
	verbose_print(' --> '), verbose_print(Nr), verbose_print(' arguments erased'),verbose_nl.

l_add_atom_of_interest(X) :- var(X),!.
l_add_atom_of_interest([]).
l_add_atom_of_interest([H|T]) :-
	add_atom_of_interest(H),
	l_add_atom_of_interest(T).

add_atom_of_interest(Atom) :-
	nonvar(Atom),
	get_sub_term(Atom,Pred,Arity,_SubT,Pos),
	assert(dont_erase(Pred,Arity,Pos)),
	debug_println(dont_erase(Pred,Arity,Pos)),
	fail.
add_atom_of_interest(_).

update_cg_filter_goal :- generate_slice_instead_of_spec_prog(no),
	retract(cg_filter_goal(NodeID,Goal,MsvGoal,FGoal)),
	pp_mnf(erase_literal(FGoal,EFGoal)),
	assert(cg_filter_goal(NodeID,Goal,MsvGoal,EFGoal)),
	fail.
update_cg_filter_goal.

calc_erased_program :- var_call_encountered,!,
    print('CALL(_) encountered, performing no erasures'),nl.
calc_erased_program :-
	reset_spec_prog,
	claus(_Nr,Head,Body),
	pp_mnf(erase_literal(Head,EHead)),
	l_erase_literal(Body,EBody),
	simple_assert_spec_clause(EHead,EBody),  /* changed from assert_spec_clause */
	fail.
calc_erased_program.


raf_analysis :-
	verbose_print('.'),
	reset_erasure_changed,
	propagate_erasure,
	(erasure_changed -> raf_analysis ; nl).
	

propagate_erasure :-
	claus(_Nr,Head,Body),
	member(Literal,Body),
	cg_extract_positive_atom_from_literal(Literal,Atom,Struct,_Ptr),
		/* POSSIBLE IMPROVEMENT: ALLOW ERASING IN SOME
		   BUILT-INS AS WELL, LIKE =, =.., is */
	propagate_erasure2(Head,Body,Atom,Struct),
	fail.
propagate_erasure.

propagate_erasure2(_Head,_Body,Atom,Struct) :-
	nonvar(Struct),
	Struct = not(_X),!, /* a negation has been encountered */
	get_sub_term(Atom,Pred,Arity,_SubTerm,Pos),	
	\+(dont_erase(Pred,Arity,Pos)),
	assert_dont_erase(Pred,Arity,Pos).
	/* keep every position for negative literals */
propagate_erasure2(_Head,_Body,Atom,_Struct) :-
	Atom = call(_),
	 /* must be var, otherwise cg_extract... would have dived further */
	(var_call_encountered -> true ; assert(var_call_encountered)).
propagate_erasure2(Head,Body,Atom,_Struct) :-
	(Atom \= call(_)),
	get_sub_term(Atom,Pred,Arity,SubTerm,Pos),	
	\+(dont_erase(Pred,Arity,Pos)), /* not erased so far */
	unsafe_erasure(SubTerm,Head,Body),
	assert_dont_erase(Pred,Arity,Pos).


assert_dont_erase(Pred,Arity,Pos) :-
	assert(dont_erase(Pred,Arity,Pos)),
	debug_println(dont_erase(Pred,Arity,Pos)),
	(erasure_changed -> true ; assert(erasure_changed)).


get_sub_term(Literal,Pred,Arity,SubTerm,Pos) :-
	Literal=.. [Pred|Args],
	length(Args,Arity),
	member_nr(SubTerm,Args,Pos).

unsafe_erasure(SubTerm,_Head,_Body) :-
	nonvar(SubTerm),!, /* it is unsafe to erase a non-variable */
	debug_print(nonvar).
unsafe_erasure(SubTerm,_Head,Body) :-
	var(SubTerm),
	l_variable_occurs(Body,SubTerm,0,Nr),
	Nr > 1,!, /* unsafe to erase if variable occurs more than once in body */
	debug_print(occ(Nr)).
unsafe_erasure(SubTerm,Head,_Body) :-
	var(SubTerm),
	erase_literal(Head,EHead),
	variable_occurs(SubTerm,EHead,Nr),
	Nr > 0,!, /* unsafe to erase if occurs in erased head */
	debug_print(head_occ).


variable_occurs(V,T,Nr) :-
	V==T,!, Nr=1.
variable_occurs(_V,T,Nr) :-
	var(T),!, Nr=0.
variable_occurs(V,T,Nr) :-
	%nonvar(T),
	T=.. [_Pred|Args],
	l_variable_occurs(Args,V,0,Nr).

l_variable_occurs([],_V,Nr,Nr).
l_variable_occurs([H|T],V,InNr,OutNr) :-
	variable_occurs(V,H,HNr),
	IntNr is InNr + HNr,
	l_variable_occurs(T,V,IntNr,OutNr).


l_erase_literal([],[]).
l_erase_literal([H|T],[EH|ET]) :-
	pp_mnf(erase_literal(H,EH)),
	l_erase_literal(T,ET).


pre_condition(erase_literal(X,_Res)) :-
	term_is_of_type(X,literal).
post_condition(erase_literal(_X,Res)) :-
	term_is_of_type(Res,literal).

erase_literal(X,Res) :-
	cg_extract_positive_atom_from_literal(X,Atom,Struct,Ptr),!,
	(Atom = call(_Call)
	 -> (Ptr = Atom /* dont erase */)
	 ;  (erase_atom(Atom,ErAtom),
	     Ptr = ErAtom
	     )
	),
	Res = Struct.
erase_literal(X,X). /* no atom inside */

erase_atom(X,Y) :-
	X =.. [Pred|Args],
	length(Args,Arity),
	erase_arguments(Args,ErArgs,Pred,Arity,1),
		/* MISSING: test whether necessary to change predicate */
		/* string_concatenate(Pred,Arity,NewPred), */
	NewPred = Pred,
	Y =.. [NewPred|ErArgs].

erase_arguments([],[],_Pred,_Arity,_ecce_bimPos).
erase_arguments([H|T],ErArgs,Pred,Arity,Pos) :-
	(dont_erase(Pred,Arity,Pos)
	 -> ErArgs = [H|ErT]
	 ;  (gen_anonymous_vars_for_erased_args(no)
	      -> ErArgs = ErT
	      ;  ErArgs = ['*'|ErT]   /* IMPROVE: maybe use special constant fo pretty printer ? */
	      ),  %print(erased(Pred,Pos)),nl,
	     inc_nr_of_erased_arguments
    ),
	P1 is Pos + 1,
	erase_arguments(T,ErT,Pred,Arity,P1).


/* ----------------------------------------------------------------- */

perform_andprint_far_analysis :-
	recalc_static_functors,
	verbose_println(' --> performing far analysis'),
	perform_far_analysis([]),
	(var_call_encountered
	 -> (print('### Warning: call(_) encountered'),nl,
	     print('### Filtered program could be incorrect !'),nl,
	     print('### Keep the original program to be safe !'),nl
	    )
	 ;  true
	),
	print_specialised_program_to_file('FAR Analysis Result').

perform_far_analysis(IGoal) :- 
	reset_dont_erase,
	reset_nr_of_erased_arguments,
	reset_var_call_encountered,
	l_add_atom_of_interest(IGoal),
	far_analysis,
	reset_nr_of_erased_arguments,
	calc_erased_program,
	update_cg_filter_goal,
	nr_of_erased_arguments(Nr),
	verbose_print(' --> '), verbose_print(Nr), verbose_println(' argument(s) erased').

far_analysis :-
	verbose_print('.'),
	reset_erasure_changed,
	propagate_far_erasure,
	(erasure_changed -> far_analysis ; nl).
	

propagate_far_erasure :-
	claus(_Nr,Head,Body),
	get_sub_term(Head,Pred,Arity,SubTerm,Pos),	
	\+(dont_erase(Pred,Arity,Pos)),
	unsafe_far_erasure(SubTerm,Head,Body),
	assert_dont_erase(Pred,Arity,Pos),
	fail.
propagate_far_erasure.


unsafe_far_erasure(SubTerm,_Head,_Body) :-
	nonvar(SubTerm),!, debug_print(far_nonvar).
unsafe_far_erasure(SubTerm,Head,_Body) :-
	var(SubTerm),
	variable_occurs(SubTerm,Head,Nr),
	Nr > 1,!, debug_print(far_head_occ(Nr)).
unsafe_far_erasure(SubTerm,_Head,Body) :-
	var(SubTerm),
	l_erase_literal(Body,EBody),
	l_variable_occurs(EBody,SubTerm,0,Nr),
	Nr > 0,!, debug_print(far_body_occ(EBody,Nr)).
