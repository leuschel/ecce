:- module('whistle.conjhomo',['whistle.conjhomo:whistle'/4]).

%:- dynamic whistle/4.

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module('../bimtools').
:- use_module('../global_tree').
:- use_module('../homeomorphic').

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

'whistle.conjhomo:whistle'(GoalID,Goal,Chtree,WhistlGoalID) :-
	gt_node_descends_from(GoalID,ParID,_LeafLocalID),
	'whistle.conjhomo:find_growing_among_ancestors'(ParID,Goal,Chtree,WhistlGoalID).


'whistle.conjhomo:find_growing_among_ancestors'(ParID,Goal,Chtree,ParID) :-
	gt_node_pe_status(ParID,pe(ImpStat)),
		/* only test with non-abstracted ancestors */
	gt_node_goal(ParID,ParGoal),
	(strict_instance_of(ParGoal,Goal)
	 -> (print('$i'),fail)
	 ;  ('whistle.conjhomo:homeomorphic_embedded_conjunction1'(ParGoal,Goal))
	).
'whistle.conjhomo:find_growing_among_ancestors'(ParID,Goal,Chtree,WhistlGoalID) :-
	gt_node_descends_from(ParID,ParID2,_LeafLocalID),
	'whistle.conjhomo:find_growing_among_ancestors'(ParID2,Goal,Chtree,WhistlGoalID).
	
'whistle.conjhomo:homeomorphic_embedded_conjunction1'(ParGoal,Goal) :-
  homeomorphic_embedded_conjunction(ParGoal,Goal), !.
