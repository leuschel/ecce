:- module('abstract.clpfd',['abstract.clpfd:abstract_parent'/6,'abstract.clpfd:abstract_leaf'/6]).

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

'abstract.clpfd:abstract_parent'(GoalID,Goal,Chtree,WhistleGoalID,
		NewWhistleGoals,NewWhistleChtrees) :-
	print(start_abstract_parent(GoalID,Goal,Chtree,WhistleGoalID,
		NewWhistleGoals,NewWhistleChtrees)),nl,
	divide_constraint_goal(Goal,OGoal,CGoal),
	gt_node_goal(WhistleGoalID,WhistleGoal),
	divide_constraint_goal(WhistleGoal,OWhistleGoal,CWGoal),
	(widen(OWhistleGoal,CWGoal,OGoal,CGoal,AGoal,AConstr)
	 ->  (print(abstract_parent(wg(WhistleGoal),widen(AGoal,AConstr))),
	     debug_nl)
	 ;   (print('abstract_parent unsuccessful'),debug_nl,fail)
	),
	append(AGoal,AConstr,NewGoal),
	NewWhistleGoals = [split_goal(NewGoal,FSI)],
	get_full_split_indicator(NewGoal,1,FSI),
	NewWhistleChtrees = [none],
	print(end_abstract_parent(GoalID,Goal,Chtree,WhistleGoalID,
		NewWhistleGoals,NewWhistleChtrees)),nl.

'abstract.clpfd:abstract_leaf'(GoalID,Goal,Chtree,WhistleGoalID,NewGoals,NewChtrees) :-
	debug_print(abstract_leaf_calling_abstract_parent),debug_nl,
	'abstract.clpfd:abstract_parent'(GoalID,Goal,Chtree,WhistleGoalID,NewGoals,NewChtrees).


get_full_split_indicator([],Nr,[]).
get_full_split_indicator([H|T],Nr,[Nr|FST]) :-
	Nr1 is Nr + 1,
	get_full_split_indicator(T,Nr1,FST).
