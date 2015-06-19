:- module('whistle.setnotmoregeneral',['whistle.setnotmoregeneral:whistle'/4]).

/* This whistle should only be used with parent abstraction enabled; otherwise we may loop */

:- use_package( .('../ecce_no_rt2') ).

%:- dynamic whistle/4.

:- set_prolog_flag(multi_arity_warnings,off).
%:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module('../bimtools').
:- use_module('../global_tree').
:- use_module('../homeomorphic').
:- use_module('../calc_chtree').


'whistle.setnotmoregeneral:whistle'(GoalID,Goal,Chtree,WhistlGoalID) :-
	% print(call_whistle(GoalID,Goal)),nl, 
	gt_node(WhistlGoalID),
	WhistlGoalID\=GoalID,
	'whistle.setnotmoregeneral:find_growing_among_ancestors'(WhistlGoalID,Goal,Chtree).


'whistle.setnotmoregeneral:find_growing_among_ancestors'(WhistlGoalID,Goal,Chtree) :-
	gt_node_pe_status(WhistlGoalID,pe(ImpStat)),
		/* only test with non-abstracted ancestors */
	gt_node_goal(WhistlGoalID,ParGoal),
	pp_cll(msg_can_be_taken(Goal,ParGoal)).
	
	/* print(test_for_growing_with(ParGoal)),nl,
	(instance_of(ParGoal,Goal)
	 -> print(par_inst_of),nl %,fail
	 ;  true
	). */
	