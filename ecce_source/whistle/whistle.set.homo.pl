:- module('whistle.set.homo',['whistle.set.homo:whistle'/4]).

:- use_package( .('../ecce_no_rt2') ).

%:- dynamic whistle/4.

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module('../bimtools').
:- use_module('../global_tree').
:- use_module('../homeomorphic').
:- use_module('../chtree_tools').
:- use_module('../calc_chtree').


'whistle.set.homo:whistle'(GoalID,Goal,Chtree,WhistlGoalID) :-
	debug_print(calling_whistle(GoalID,Goal)),debug_nl,
	gt_node(WhistlGoalID),
	WhistlGoalID\=GoalID,
	'whistle.set.homo:find_growing_among_ancestors'(WhistlGoalID,Goal,Chtree).


'whistle.set.homo:find_growing_among_ancestors'(WhistlGoalID,Goal,Chtree) :-
	gt_node_pe_status(WhistlGoalID,pe(ImpStat)),
		/* only test with non-abstracted ancestors */
	gt_node_goal(WhistlGoalID,ParGoal),
	pp_cll(msg_can_be_taken(Goal,ParGoal)),
	debug_print(test_for_growing_with(ParGoal)),debug_nl,
	(instance_of(ParGoal,Goal)
	 -> debug_print(par_inst_of),debug_nl,fail
	 ;  true
	),
	homeomorphic_embedded(ParGoal,Goal),
	debug_print('atom growing detected'),debug_nl,
	gt_node_chtree(WhistlGoalID,ParChtree),
	pp_mnf(transform_chtree_into_chterm(Chtree,Chterm)),
	pp_mnf(transform_chtree_into_chterm(ParChtree,ParChterm)),
	(homeomorphic_embedded(ParChterm,Chterm)
	 -> true ; (debug_print('no chtree growing'),debug_nl,
		     fail)
	),
	print('*'),debug_print('chtree growing detected*'),
	debug_print(Goal),debug_print(' grows from '), debug_print(ParGoal),
	((Chtree=ParChtree) -> debug_print(' with same chtree ') ; true),
	debug_nl.
	
