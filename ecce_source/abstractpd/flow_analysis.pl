:- module(flow_analysis,_).

:- use_package( .('../ecce_no_rt2') ).

/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 2001           */
/* --------------------------------------------- */

/* TO GET THE LATEST VERSION OF THE ECCE SYSTEM GO TO:
	http://www.ecs.soton.ac.uk/~mal */
/* BUG REPORTS, QUESTIONS AND COMMENTS CAN BE SENT TO:
	mal@ecs.soton.ac.uk */

/* file: abstractpd/calc_chtree.pl */

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module('../bimtools').

:- use_module('../global_tree').
:- use_module('../calc_chtree').
:- use_module('../main_functions').
%:- use_module(calc_chtree).
:- use_module(calc_chtree_pd).

:- include( '../multi_meta' ).


/* ===================================================== */


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

/* ===================================================== */



abstract_partial_deduction_enabled(yes).

/* ------------------ */
/*   flow_analysis/1  */
/* ------------------ */

/* overrides flow_analysis in ecce_main.pro */

td_cflow_analysis(Count) :-
	((Count > 10)
	 -> (NewCount = 1, get_nr_of_gt_nodes_to_pe(NrOfNPE),
	     print('['),print(NrOfNPE),print(']'))
	 ;  (NewCount is Count + 1)
	),
	pp_cll(get_gt_cgoal_to_pe(GoalID,Goal,Constraint)),!,
	copy(Goal,G),numbervars(G,1,_),
	debug_print(goal_to_pe(GoalID,G,Constraint)),debug_nl,
	(gt_node_chtree(GoalID,none)
	 -> (find_unimposed_variant(GoalID,Goal,VariantID)
	     -> (debug_print(found_variant(VariantID,Goal)),debug_nl,
			/* no need to unfold */
		     gt_node_chtree(VariantID,Chtree), /* just get chtree */
		     ImpStat = unimposed
		    )
	     ;  (/* print(Goal),nl, */
	         print(unfolding(Goal)),nl,
		 time(pp_mnf(
		    calculate_chtree(GoalID,Goal,Constraint,Chtree)),UnfTime),
		 add_unf_time(UnfTime),
		 debug_print(finished_unfolding(Goal)), debug_nl,
		 debug_print_chtree(Chtree),
		 ImpStat = unimposed
		)
	    )
	 ;  (debug_print(handling(Goal)),debug_nl,  /* no need to unfold */
		gt_node_chtree(GoalID,ImpChtree), /* just get imposed chtree */
		copy(Goal,CCGoal),
	        pp_mnf(remove_incorrect_builtins(ImpChtree,CCGoal,
						CorrectedChtree)),
		copy(Goal,CPPGoal),
		pp_mnf(post_prune_chtree(CPPGoal,CorrectedChtree,PChtree)),
		((PChtree = stop)
		 -> (debug_print(one_step_unfolding(GoalID,Goal,Constraint)),
		     debug_nl,
		     pp_mnf(one_step_unfolding(GoalID,Goal,Constraint,Chtree)))
		 ;  (Chtree = PChtree)
		),
		ImpStat = imposed
	    )
	),!,
	((Chtree=stop) -> (set_abnormal_goal_encountered) ; (true)),
	/* print_chtree(Chtree), */
	(pp_cll(get_cinstance_of(GoalID,Goal,Constraint,Chtree,MoreGeneralID))
	 -> (debug_print(cinstance_of(MoreGeneralID)),debug_nl,
	     pp_mnf(mark_gt_node_as_instance_of(GoalID,MoreGeneralID)),
	     pp_mnf(mark_gt_node_as_ped(GoalID,pe(ImpStat),Chtree)),
	     print('+'), debug_nl
	    )
         ;  (pp_cll(whistle(GoalID,Goal,Chtree,WhistleGoalID))
	         -> (debug_print(abstracting(WhistleGoalID))),debug_nl,
	             abstract_and_replace(GoalID,Goal,Chtree,WhistleGoalID,ImpStat),
	             debug_print(done_abstracting),debug_nl,
	            )
	        ;  (pp_mnf(mark_gt_node_as_ped(GoalID,pe(ImpStat),Chtree)),
	            debug_print(adding_leaves),debug_nl,
		        add_leaves(GoalID,Goal,Constraint,Chtree),
	            debug_print(done_adding_leaves),debug_nl,
		        verbose_print('.')
		       )
	     )
	 ),!,
	 debug_nl,debug_print('...'),debug_nl,
	td_cflow_analysis(NewCount).
td_cflow_analysis(_) :- nl.


flow_analysis(C) :-
       print_gt_node_bup,
       reset_bup_msg_changed,
       print('Starting Top-Down Iteration'),nl,
       td_cflow_analysis(C),
       print_gt_node_bup,
       print('Starting Bottom-Up Iteration'),nl,
       recompute_leaves(_),
       (( bup_msg_changed(_) ;
          pp_cll(get_gt_cgoal_to_pe(_,_,_)))
        -> (flow_analysis(C))
	;  (print('Fixpoint reached'),nl)).

find_unimposed_cinstance(Goal,Constraint,VariantID) :-
	find_unimposed_cinstance(root,Goal,Constraint,VariantID).
find_unimposed_cinstance(GoalID,Goal,Constraint,VariantID) :-
	copy(Goal,CGoal),
	gt_node_goal(VariantID,CGoal), /* lookup a potential match */
	not(GoalID = VariantID),
	gt_node_pe_status(VariantID,VPEStat),
	not(VPEStat = no),
		/* VariantID should already be PE'd */
	not(VPEStat = pe(imposed)), not(VPEStat = abstracted(imposed)),
		/* otherwise chtree might be incorrect */
	gt_node_constraint(VariantID,(MoreGeneralGoal,MC)),
	pp_cll(constraint_instance_of(Goal,Constraint,MoreGeneralGoal,MC)).
	
	
	


/* --------- For Abstract Partial Deduction: -------------- */

pre_condition(get_cinstance_of(GoalID,Goal,C,Chtree,_MoreGeneralID)) :-
	term_is_of_type(GoalID,nodeid),
	term_is_of_type(Goal,goal),
	term_is_of_type(C,constraint),
	term_is_of_type(Chtree,chtree).
post_condition(get_cinstance_of(_GoalID,_Goal,_C,_Chtree,MoreGeneralID)) :-
	term_is_of_type(MoreGeneralID,nodeid).

get_cinstance_of(GoalID,Goal,Constraint,Chtree,MoreGeneralID) :-
	Chtree \== stop, /* this will happen for goals like [X] */
	copy(Goal,CGoal),
	numbervars(CGoal,1,_),
	gt_node_goal(MoreGeneralID,CGoal),
	GoalID \== MoreGeneralID,
	gt_node_pe_status(MoreGeneralID,PEStatus),
	PEStatus \== no,
	instance_is_ok(MoreGeneralID,PEStatus,Chtree),
	gt_node_constraint(MoreGeneralID,(MG,MGC)),
        constraint_instance_of(Goal,Constraint,MG,MGC). 

instance_is_ok(_MoreGeneralID,PEStat,_Chtree) :-
	PEStat \== pe(imposed), PEStat \== abstracted(imposed).
		/* otherwise chtree might be incorrect */
instance_is_ok(MoreGeneralID,_PEStat,Chtree) :-
	gt_node_chtree(MoreGeneralID,Chtree).