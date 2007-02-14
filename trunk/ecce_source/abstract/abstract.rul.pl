:- module('abstract.rul',['abstract.rul:abstract_parent'/6,'abstract.rul:abstract_leaf'/6]).

%:- use_module('../rul/INTERFACE').
%%%:-ecce_use_module('rul/ecceRUL').

%:- initialization(retractall(rul_active(_)),assert(rul_active(yes))).

%:- dynamic abstract_parent/6.
%:- dynamic abstract_leaf/6.

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../global_tree').
/*
:- multifile gt_node/1,gt_node_goal/2,gt_node_constraint/2,
             gt_node_bup_cas/3,gt_node_descends_from/3,
             gt_node_instance_of/2,gt_node_chtree/2,gt_node_pe_status/2,
	     gt_node_user_info/2.             

:- dynamic gt_node/1.
:- dynamic gt_node_goal/2.
:- dynamic gt_node_constraint/2.
:- dynamic gt_node_bup_cas/3.
:- dynamic gt_node_descends_from/3.
:- dynamic gt_node_instance_of/2.
:- dynamic gt_node_chtree/2.
:- dynamic gt_node_pe_status/2.
:- dynamic gt_node_user_info/2.
*/

/* TO DO: below will never split: use homeo to do classical splitting
 first, then apply widening */

'abstract.rul:abstract_parent'(GoalID,Goal,Chtree,WhistleGoalID,
		NewWhistleGoals,NewWhistleChtrees) :-
    print('------------ ABSTRACT ---------'),nl,
	print(start_rul_abstract_parent(GoalID,Goal,Chtree,WhistleGoalID,
		NewWhistleGoals,NewWhistleChtrees)),nl,
	divide_constraint_rul_goal(Goal,OGoal,CGoal),
	gt_node_goal(WhistleGoalID,WhistleGoal),
	divide_constraint_rul_goal(WhistleGoal,OWhistleGoal,CWGoal),
	print(calling_ecceRUL_widen(OWhistleGoal,OGoal)),nl,
	/* tell('ecce_rul_widen_output'),*/
	print(calling_ecceRUL_widen(OWhistleGoal,CWGoal,OGoal,CGoal,AGoal,AConst)),nl,/* told, */
	(widen(OWhistleGoal,CWGoal,OGoal,CGoal,AGoal,AConstr)
	 ->  (print(widened_ordinary_goal(AGoal)),debug_nl,
	      (true /*trace_printing(on)*/ -> print_rul(AConstr) ; true),
	      debug_nl)
	 ;   (print('widen failed: abstract_parent unsuccessful'),debug_nl,fail)
	),
	analyticFold:l_goalRULification(AGoal,NewAGoal,AConstr,NewAConstr),
	print(rULified_widened_ordinary_goal(NewAGoal)),debug_nl,
	(true /* trace_printing(on)*/ -> print_rul(NewAConstr) ; true),
	append(NewAGoal,[NewAConstr],NewGoal),nl,nl,
	print_goal_for_dot(NewGoal),nl,
	NewWhistleGoals = [split_goal(NewGoal,FSI)],
	get_full_split_indicator(NewGoal,1,FSI), debug_print(fsi(FSI)),debug_nl,
	NewWhistleChtrees = [none],
	print(end_abstract_parent(GoalID,Goal,Chtree,WhistleGoalID,
		NewWhistleGoals,NewWhistleChtrees)),nl,nl,
	print(w),debug_nl.

'abstract.rul:abstract_leaf'(GoalID,Goal,Chtree,WhistleGoalID,NewGoals,NewChtrees) :-
	debug_print(abstract_leaf_calling_abstract_parent),debug_nl,
	'abstract.rul:abstract_parent'(GoalID,Goal,Chtree,WhistleGoalID,NewGoals,NewChtrees).


get_full_split_indicator([],Nr,[]).
get_full_split_indicator([H|T],Nr,[Nr|FST]) :-
	Nr1 is Nr + 1,
	get_full_split_indicator(T,Nr1,FST).
