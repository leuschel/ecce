:- module('whistle.conj-termsize',['whistle.conj-termsize:whistle'/4]).

:- use_package( .('../ecce_no_rt2') ).

%:- dynamic whistle/4.

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module('../bimtools').
:- use_module('../global_tree').
:- use_module('../homeomorphic').


:- use_module( 'whistle.common.pl' ).

'whistle.conj-termsize:whistle'(GoalID,Goal,Chtree,WhistlGoalID) :-
	gt_node_descends_from(GoalID,ParID,_LeafLocalID),
	'whistle.conj-termsize:find_growing_among_ancestors'(ParID,Goal,Chtree,WhistlGoalID).


'whistle.conj-termsize:find_growing_among_ancestors'(ParID,Goal,Chtree,ParID) :-
	gt_node_pe_status(ParID,pe(ImpStat)),
		/* only test with non-abstracted ancestors */
	gt_node_goal(ParID,ParGoal),
	not(strict_instance_of(ParGoal,Goal)),
	not_more_general_conjunction1(ParGoal,Goal),
	debug_print('conjunction growing detected'),debug_nl.
'whistle.conj-termsize:find_growing_among_ancestors'(ParID,Goal,Chtree,WhistlGoalID) :-
	gt_node_descends_from(ParID,ParID2,_LeafLocalID),
	'whistle.conj-termsize:find_growing_among_ancestors'(ParID2,Goal,Chtree,WhistlGoalID).
	
