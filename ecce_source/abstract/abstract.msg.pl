:- module('abstract.msg',['abstract.msg:abstract_parent'/6,'abstract.msg:abstract_leaf'/6]).

%:- dynamic abstract_parent/6.
%:- dynamic abstract_leaf/6.

:- set_prolog_flag(multi_arity_warnings,off).
%:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../global_tree').

:- use_module( 'abstract.common.pl' ).


'abstract.msg:abstract_parent'(GoalID,Goal,Chtree,WhistleGoalID,
		NewWhistleGoals,NewWhistleChtrees) :- 
	gt_node_goal(WhistleGoalID,WhistleGoal),
	msg(Goal,WhistleGoal,MSG),
	(variant_of(MSG,WhistleGoal)
	 -> (debug_print('abstract_parent unsuccessful'),debug_nl,fail)
	 ;  (debug_print(abstract_parent(wg(WhistleGoal),msg(MSG))),debug_nl)
	),
	NewWhistleGoals = [split_goal(MSG,FSI)],
	get_full_split_indicator(MSG,1,FSI),
	NewWhistleChtrees = [none].

'abstract.msg:abstract_leaf'(GoalID,Goal,Chtree,WhistleGoalID,NewGoals,NewChtrees) :-
	gt_node_goal(WhistleGoalID,WhistleGoal),
	msg(Goal,WhistleGoal,MSG),
	NewGoals = [split_goal(MSG,FSI)],
	get_full_split_indicator(MSG,1,FSI),
	NewChtrees = [none].

