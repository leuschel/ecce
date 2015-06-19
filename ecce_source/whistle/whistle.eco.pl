:- module('whistle.eco',['whistle.eco:whistle'/4]).

:- use_package( .('../ecce_no_rt2') ).

%:- dynamic whistle/4.

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module('../bimtools').
:- use_module('../global_tree').
:- use_module('../homeomorphic').
:- use_module('../calc_chtree').


'whistle.eco:whistle'(GoalID,Goal,Chtree,WhistlGoalID) :-
	(Chtree\=empty),
	gt_node_descends_from(GoalID,ParID,_LeafLocalID),
	'whistle.eco:find_growing_among_ancestors'(ParID,Goal,Chtree,WhistlGoalID).


'whistle.eco:find_growing_among_ancestors'(ParID,Goal,Chtree,ParID) :-
	gt_node_pe_status(ParID,pe(ImpStat)),
		/* only test with non-abstracted ancestors */
	gt_node_chtree(ParID,Chtree), /* have to have the same chtree */
	gt_node_goal(ParID,ParGoal),
	pp_cll(msg_can_be_taken(Goal,ParGoal)),
		/* for standard pd ensured by same chtree */
	(\+(instance_of(ParGoal,Goal))
	 -> true /* blow whistle */
	 ;  instance_of(Goal,ParGoal) /* we have a variant */
	).
'whistle.eco:find_growing_among_ancestors'(ParID,Goal,Chtree,WhistlGoalID) :-
	gt_node_descends_from(ParID,ParID2,_LeafLocalID),
	'whistle.eco:find_growing_among_ancestors'(ParID2,Goal,Chtree,WhistlGoalID).
	
