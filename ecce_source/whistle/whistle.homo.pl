:- module('whistle.homo',['whistle.homo:whistle'/4]).

:- use_package( .('../ecce_no_rt2') ).

%:- dynamic whistle/4.

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module('../bimtools').
:- use_module('../global_tree').
:- use_module('../homeomorphic').
:- use_module('../calc_chtree').
:- use_module('../chtree_tools').


'whistle.homo:whistle'(GoalID,Goal,Chtree,WhistlGoalID) :-
	debug_print(calling_whistle(GoalID,Goal)),debug_nl,
	gt_node_descends_from(GoalID,ParID,_LeafLocalID),
	'whistle.homo:find_growing_among_ancestors'(ParID,Goal,Chtree,WhistlGoalID).


'whistle.homo:find_growing_among_ancestors'(ParID,Goal,Chtree,ParID) :-
	gt_node_pe_status(ParID,pe(ImpStat)),
		/* only test with non-abstracted ancestors */
	gt_node_goal(ParID,ParGoal),
	pp_cll(msg_can_be_taken(Goal,ParGoal)),
	debug_print(test_for_growing_with(ParGoal)),debug_nl,
	(strict_instance_of(ParGoal,Goal)
	 -> (debug_print(par_strict_inst_of),debug_nl,fail)
	 ;  true
	),
	homeomorphic_embedded(ParGoal,Goal),
	debug_print('atom growing detected'),debug_nl,
	gt_node_chtree(ParID,ParChtree),
	debug_print(parent),debug_nl,
	pp_mnf(transform_chtree_into_chterm(Chtree,Chterm)),
	pp_mnf(transform_chtree_into_chterm(ParChtree,ParChterm)),
	/* term_nesting_level(Chterm,ChtLevel,ChtSum),
	print(c(ChtLevel,ChtSum)),nl,*/
	(chtree_homeomorphic_embedded(ParChterm,Chterm)
	 -> true ; (print('c'),
		     debug_print('no chtree growing '),debug_nl,
		     debug_print('parent: '),debug_nl,
		     debug_print_chtree(ParChtree),
		     /* print(ParChterm),debug_nl, */
		     debug_print('growing child:'),debug_nl,
		     debug_print_chtree(Chtree),
		     /* print(Chterm),debug_nl, */
		     fail)
	),
	debug_print('*chtree growing detected*'),
	debug_print(Goal),debug_print(' desc from '),debug_print(ParGoal),
	((Chtree=ParChtree) -> debug_print(' with same chtree ') ; true),
	debug_nl.
'whistle.homo:find_growing_among_ancestors'(ParID,Goal,Chtree,WhistlGoalID) :-
	gt_node_descends_from(ParID,ParID2,_LeafLocalID),
	'whistle.homo:find_growing_among_ancestors'(ParID2,Goal,Chtree,WhistlGoalID).
	
