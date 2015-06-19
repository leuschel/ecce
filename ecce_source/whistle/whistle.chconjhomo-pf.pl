:- module('whistle.chconjhomo-pf',['whistle.chconjhomo-pf:whistle'/4]).

:- use_package( .('../ecce_no_rt2') ).

%:- dynamic whistle/4.

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module('../bimtools').
:- use_module('../global_tree').
:- use_module('../homeomorphic').
:- use_module('../chtree_tools').

:- use_module( 'whistle.common.pl' ).


'whistle.chconjhomo-pf:whistle'(GoalID,Goal,Chtree,WhistlGoalID) :-
	gt_node_descends_from(GoalID,ParID,_LeafLocalID),
	'whistle.chconjhomo-pf:find_growing_among_ancestors'(GoalID,ParID,Goal,Chtree,WhistlGoalID).


'whistle.chconjhomo-pf:find_growing_among_ancestors'(GoalID,ParID,Goal,Chtree,ParID) :-
	gt_node_pe_status(ParID,pe(ImpStat)),
		/* only test with non-abstracted ancestors */
	gt_node_goal(ParID,ParGoal),
	\+(strict_instance_of(ParGoal,Goal)),
		   pp_mnf(calc_pruning_factor(Chtree,PFC)),
		   debug_print(pf(PFC)),debug_nl,
	gt_node_chtree(ParID,ParChtree),
		   pp_mnf(calc_pruning_factor(ParChtree,ParPFC)),
		   debug_print(pf_par(ParPFC)),debug_nl,
	not_more_general_conjunction1(ParGoal,Goal),
	(sufficient_specialisation(PFC,ParPFC)
	 -> 
	  (
	   'whistle.chconjhomo-pf:homeomorphic_embedded_conjunction1'(ParGoal,Goal),
	   debug_print('conjunction growing detected'),debug_nl,
	   pp_mnf(transform_chtree_into_chterm(Chtree,Chterm)),
	   pp_mnf(transform_chtree_into_chterm(ParChtree,ParChterm)),
	   (chtree_homeomorphic_embedded(ParChterm,Chterm)
	    -> true ; (print('c'),debug_print(' '),
		     debug_print('no chtree growing'),debug_nl,
		     fail)
	   ),
	   debug_print('*chtree growing detected*'),debug_nl,
	   ((gt_node_user_info(ParID,unpruned_chtree(UParChtree)),
	     gt_node_user_info(GoalID,unpruned_chtree(UChtree)),
	     (UParChtree \== ParChtree) ; (UChtree \== Chtree) )
	    -> (debug_print(trying_unpruned),debug_nl,
	        pp_mnf(transform_chtree_into_chterm(UChtree,UChterm)),
	        pp_mnf(transform_chtree_into_chterm(UParChtree,UParChterm)),
	        (chtree_homeomorphic_embedded(UParChterm,UChterm)
	 	   -> true
		   ; (print('u'),debug_print(' '),
		      debug_print('no unpruned chtree growing'),debug_nl,
		      fail)
	        )
	       )
	    ;  true
	   )
	 )
	; (print('p'),debug_print(pf(PFC,ParPFC)),
	   debug_print('pruning factor insufficient'),debug_nl)
	),
	debug_print(Goal),debug_print(' desc from '), debug_print(ParGoal),
	((Chtree=ParChtree)
		-> debug_print(' with same chtree ')
		;  true
	),
	debug_nl.
'whistle.chconjhomo-pf:find_growing_among_ancestors'(GoalID,ParID,Goal,Chtree,WhistlGoalID) :-
	gt_node_descends_from(ParID,ParID2,_LeafLocalID),
	'whistle.chconjhomo-pf:find_growing_among_ancestors'(GoalID,ParID2,Goal,Chtree,WhistlGoalID).
	
'whistle.chconjhomo-pf:homeomorphic_embedded_conjunction1'(ParGoal,Goal) :-
  homeomorphic_embedded_conjunction(ParGoal,Goal), !.

sufficient_specialisation(PFC,ParPFC) :-
	(PFC >= 0.70) ; (ParPFC >= 0.70) ; ((PFC-ParPFC)>= 0.40).



