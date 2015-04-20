:- module('abstract.eco',['abstract.eco:abstract_parent'/6,'abstract.eco:abstract_leaf'/6]).

%:- dynamic abstract_parent/6.
%:- dynamic abstract_leaf/6.

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../global_tree').

:- use_module( 'abstract.common.pl' ).

'abstract.eco:abstract_parent'(GoalID,Goal,Chtree,WhistleGoalID,
		NewWhistleGoals,NewWhistleChtrees) :- fail.

'abstract.eco:abstract_leaf'(GoalID,Goal,Chtree,WhistleGoalID,NewGoals,NewChtrees) :-
	gt_node_goal(WhistleGoalID,WhistleGoal),
	(gt_node_pe_status(WhistleGoalID,no)
	 -> (print('### WARNING improper use of abstract.eco'),nl,
	     print('### Whistle Goal not partially evaluated !'),nl
	    )
	 ;  (true)
	),
	gt_node_chtree(WhistleGoalID,WhistleChtree),
	((Chtree=WhistleChtree)
	 -> (NewChtrees = [Chtree])
	 ;  (print('### WARNING: improper use of abstract.eco'),nl,
	     print('### chtree of goals not identical !'),nl,
	     print('### Goal = '), print(Goal),nl,
	     print_chtree(Chtree),nl,
	     print('### WhistleGoal = '), print(WhistleGoal),nl,
	     print_chtree(WhistleChtree),nl,
	     NewChtrees = [none])
	),
	msg(Goal,WhistleGoal,MSG),
	NewGoals = [split_goal(MSG,FSI)],
	get_full_split_indicator(MSG,1,FSI).
