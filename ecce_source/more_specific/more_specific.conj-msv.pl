:- module('more_specific.conj-msv',[
	'more_specific.conj-msv:more_specific_transformation'/1
%	,get_foldable_instances/4, % commented out by mal
%	unfold_once/2
]).

:- use_package( .('../ecce_no_rt2') ).


%:- dynamic more_specific_transformation/1.

:- set_prolog_flag(multi_arity_warnings,off).
%:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../msv_analysis').

:- use_module( mst ).

/* instantiates goals during the unfolding process to more specific versions */

'more_specific.conj-msv:more_specific_transformation'(Goal) :-
	l_peel_off_calls(CGoal,Goal),
	print(calling(msv(Goal))),nl,
	get_fold_clauses(Goal,FClauses),
	print(gfc(Goal,FClauses)),nl,
	fold_bup(FClauses,[],T),print(fold_bup(T)),nl,
	T = [Goal].

fold_bup(FClauses,OldT,NewT) :-
	fold_bup_propagation_step(FClauses,OldT,NewGs),
	%take_msg(NewGs,[],IntT),  <<< changed by MV >>>
	take_msg(NewGs,IntT),
	(variant_of(OldT,IntT)
	-> (NewT = IntT)
	;  (print('.'),fold_bup(FClauses,IntT,NewT))
	).

fold_bup_propagation_step([],Tuple,Tuple).
fold_bup_propagation_step([fold_clause(Goal,Fold,Rest)|T],Tuple,NewGs) :-
	copy(fold_clause(Goal,Fold,Rest),fold_clause(CG,CF,CR)),
	debug_print(treating(fold_clause(Goal,Fold,Rest))),debug_nl,
	((propagate_fold(CF,Tuple),
	  debug_print(propagate_fold(CF)),debug_nl,
	  msv_of_goal(CR),
	  debug_print(msv_of_goal(CR)),debug_nl)
	 -> (NewGs = [CG|RNew])
	 ;  (NewGs = RNew)
	),
	fold_bup_propagation_step(T,Tuple,RNew).

propagate_fold([],Tuple).
propagate_fold([FGoal|T],Tuple) :-
	member(FG,Tuple),copy(FG,FGC),
	FGoal=FGC,!,
	propagate_fold(T,Tuple).


get_fold_clauses(Goal,FClauses) :-
	bd_findall(fold_clause(Goal,Fold,Rest),
		(unfold_once(Goal,Body),
		 get_foldable_instances(Goal,Body,Fold,Rest)),FClauses).

get_foldable_instances(Goal,Body,Instances,Rest) :-
	((Goal \==[], find_foldable_instance(Goal,Body,Inst1,Rest1))
	 -> (Instances = [Inst1|RI],
	     get_foldable_instances(Goal,Rest1,RI,Rest)
	    )
	 ;  (Instances = [], Rest = Body)
	).

find_foldable_instance([],Body,[],Body).
find_foldable_instance([Atom|T],Body,[FoldableAtom|FT],RemBody) :-
	member_nr(FoldableAtom,Body,Nr),
	instance_of(FoldableAtom,Atom),
	pp_mnf(split_list(Body,Nr,Left,FoldableAtom,Right)),
	append(Left,Right,Rest),
	find_foldable_instance(T,Rest,FT,RemBody).
	

unfold_once([],[]).
unfold_once([H|T],Res) :-
	unfold_literal_once(H,UH),
	append(UH,UT,Res),
	unfold_once(T,UT).

unfold_literal_once(Lit,[]) :-
	is_callable_built_in_literal(Lit),!,
	call_built_in(BI).
unfold_literal_once(Lit,[Lit]) :-
	is_built_in_literal(Lit),!.
unfold_literal_once(Lit,[Lit]) :-
	is_negative_literal(Lit,Atom),!.
unfold_literal_once(Lit,Body) :-
	claus(_Nr,Lit,Body).
