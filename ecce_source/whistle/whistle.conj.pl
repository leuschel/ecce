:- module('whistle.conj',['whistle.conj:whistle'/4]).

/* file: whistle.chconj.pro */

:- dynamic whistle/4.

%not(Goal) :- \+(Goal).


'whistle.conj:whistle'(GoalID,Goal,Chtree,WhistlGoalID) :-
	gt_node_descends_from(GoalID,ParID,_LeafLocalID),
	'whistle.conj:find_growing_among_ancestors'(ParID,Goal,Chtree,WhistlGoalID).


'whistle.conj:find_growing_among_ancestors'(ParID,Goal,Chtree,ParID) :-
	gt_node_pe_status(ParID,pe(ImpStat)),
		/* only test with non-abstracted ancestors */
	gt_node_goal(ParID,ParGoal),
	not(strict_instance_of(ParGoal,Goal)),
	debug_print('conjunction growing detected'),debug_nl,
	gt_node_chtree(ParID,ParChtree),
	pp_mnf(transform_chtree_into_chterm(Chtree,Chterm)),
	pp_mnf(transform_chtree_into_chterm(ParChtree,ParChterm)),
	(chtree_homeomorphic_embedded(ParChterm,Chterm)
	 ->(true) ; (debug_print('no chtree growing'),debug_nl,
		     fail)
	),
	debug_print('*chtree growing detected*'),
	debug_print(Goal),debug_print(' desc from '), debug_print(ParGoal),
	((Chtree=ParChtree)
		-> debug_print(' with same chtree ')
		;  (true)
	),
	debug_nl.
'whistle.conj:find_growing_among_ancestors'(ParID,Goal,Chtree,WhistlGoalID) :-
	gt_node_descends_from(ParID,ParID2,_LeafLocalID),
	'whistle.conj:find_growing_among_ancestors'(ParID2,Goal,Chtree,WhistlGoalID).
	
