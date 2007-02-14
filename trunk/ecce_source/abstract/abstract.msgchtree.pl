:- module('abstract.msgchtree',['abstract.msgchtree:abstract_parent'/6,'abstract.msgchtree:abstract_leaf'/6]).

:- use_package( .('../ecce_no_rt2') ).



%:- dynamic abstract_parent/6.
%:- dynamic abstract_leaf/6.

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../chtree_tools').
:- use_module('../global_tree').

:- use_module( 'abstract.common.pl' ).

'abstract.msgchtree:abstract_parent'(GoalID,Goal,Chtree,WhistleGoalID,
		NewGoals,NewChtrees) :- 
	gt_node_pe_status(GoalID,PEStatus),
	debug_print(calling_abstract(GoalID,PEStatus,Goal)),debug_nl,
	gt_node_goal(WhistleGoalID,WhistleGoal),
	gt_node_pe_status(WhistleGoalID,WPEStatus),
	debug_print(abstracting_with(WhistleGoalID,WPEStatus,
			WhistleGoal)),debug_nl,
	gt_node_chtree(WhistleGoalID,WhistleChtree),
	msg(Goal,WhistleGoal,MSG),
	((Chtree=WhistleChtree)
	 -> (NewChtrees = [Chtree])
	 ;  (pp_mnf(msg_chtree(Chtree,WhistleChtree,MSGChtree)),
	     ((MSGChtree=stop, not(variant_of(WhistleGoal,MSG)))
		/* if MSG strictly more general: no danger of non-termination */
	      -> (NewChtrees = [none]) /* so that the goal will be unfolded */
	      ;  (NewChtrees = [MSGChtree])
	     )
	    )
	),
	(not(variant_of(WhistleGoal,MSG)) ; (NewChtrees \== [WhistleChtree])),
		/* abstract_parent must change something */
	NewGoals = [split_goal(MSG,FSI)],
	get_full_split_indicator(MSG,1,FSI).

'abstract.msgchtree:abstract_leaf'(GoalID,Goal,Chtree,WhistleGoalID,NewGoals,NewChtrees) :-
	gt_node_pe_status(GoalID,PEStatus),
	debug_print(calling_abstract(GoalID,PEStatus,Goal)),debug_nl,
	gt_node_goal(WhistleGoalID,WhistleGoal),
	gt_node_pe_status(WhistleGoalID,WPEStatus),
	debug_print(abstracting_with(WhistleGoalID,WPEStatus,
			WhistleGoal)),debug_nl,
	gt_node_chtree(WhistleGoalID,WhistleChtree),
	msg(Goal,WhistleGoal,MSG),
	((Chtree=WhistleChtree)
	 -> (NewChtrees = [Chtree])
	 ;  (pp_mnf(msg_chtree(Chtree,WhistleChtree,MSGChtree)),
	     ((MSGChtree=stop, not(variant_of(Goal,MSG)))
		/* if MSG strictly more general: no danger of non-termination */
	      -> (NewChtrees = [none]) /* so that the goal will be unfolded */
	      ;  (NewChtrees = [MSGChtree])
	     )
	    )
	),
	NewGoals = [split_goal(MSG,FSI)],
	get_full_split_indicator(MSG,1,FSI).
