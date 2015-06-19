:- module('abstract.precisemsgchtree',['abstract.precisemsgchtree:abstract_parent'/6,
	                               'abstract.precisemsgchtree:abstract_leaf'/6]).

:- use_package( .('../ecce_no_rt2') ).

%:- dynamic abstract_parent/6.
%:- dynamic abstract_leaf/6.

:- set_prolog_flag(multi_arity_warnings,off).
%:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../chtree_tools').
:- use_module('../global_tree').

:- use_module( 'abstract.common.pl' ).

'abstract.precisemsgchtree:abstract_parent'(GoalID,Goal,Chtree,WhistleGoalID,
		NewWhistleGoals,NewWhistleChtrees) :- fail.

'abstract.precisemsgchtree:abstract_leaf'(GoalID,Goal,Chtree,WhistleGoalID,NewGoals,NewChtrees) :-
	gt_node_goal(WhistleGoalID,WhistleGoal),
	gt_node_chtree(WhistleGoalID,WhistleChtree),
	msg(Goal,WhistleGoal,MSG),
	((Chtree=WhistleChtree)
	 -> (NewChtrees = [Chtree])
	 ;  (pp_mnf(precise_msg_chtree(Chtree,WhistleChtree,MSGChtree)),
	     ((MSGChtree=stop, \+(variant_of(Goal,MSG)))
	      -> (NewChtrees = [none]) /* so that the goal will be unfolded */
	      ;  (NewChtrees = [MSGChtree])
	     )
	    )
	),
	NewGoals = [split_goal(MSG,FSI)],
	get_full_split_indicator(MSG,1,FSI).

