:- module( 'post_prune.common' , _ ).

:- set_prolog_flag(single_var_warnings,off).

:- use_module( '../bimtools' ).
:- use_module('../calc_chtree').
:- use_module('../more_specific/more_specific').


/* post_prune_chtree_minlvs(G,T,C,OC,NrLvs) :-
	print(call_post_prune_chtree_minlvs(G,T,C)),nl,fail. */
post_prune_chtree_minlvs(Goal,Top,success,success,0).
post_prune_chtree_minlvs(Goal,Top,stop,stop,L) :-
	partition_goal(Goal,SplitGoals),
	length(SplitGoals,L).
post_prune_chtree_minlvs(Goal,Top,empty,empty,0).
post_prune_chtree_minlvs(Goal,Top,remove(SelLitNr,Pred,Children),
				PrunedChtree,NrLeaves) :-
	PrunedChtree = remove(SelLitNr,Pred,PrunedChildren),
	pp_mnf(more_specific_transformation(Goal)),
	pp_mnf(split_list(Goal,SelLitNr,Left,Sel,Right)),
	pp_mnf(append(Left,Right,NewGoal)),
	post_prune_chtree_minlvs(NewGoal,no,Children,PrunedChildren,NrLeaves).
post_prune_chtree_minlvs(Goal,Top,built_in_eval(SelLitNr,BI,Children),
				PrunedChtree,NrLeaves) :-
	PrunedChtree = built_in_eval(SelLitNr,BI,PrunedChildren),
	pp_mnf(more_specific_transformation(Goal)),
	pp_mnf(split_list(Goal,SelLitNr,Left,Sel,Right)),
	(is_callable_built_in_literal(Sel) -> call_built_in(Sel) ; true),
	pp_mnf(append(Left,Right,NewGoal)),
	post_prune_chtree_minlvs(NewGoal,no,Children,PrunedChildren,NrLeaves).
post_prune_chtree_minlvs(Goal,Top,select(SelLitNr,Chpaths),PrunedChtree,NrLeaves) :-
	pp_mnf(more_specific_transformation(Goal)),
	pp_mnf(split_list(Goal,SelLitNr,Left,Sel,Right)),
	partition_goal(Goal,SplitGoals),
	length(SplitGoals,NrSplitGoals),
	post_prune_chpaths_minlvs(Left,Sel,Right,Chpaths,
						PrunedChpaths,ChildLeaves),
	((ChildLeaves > NrSplitGoals,
	  Top=no)
	 -> (PrunedChtree = stop, NrLeaves = NrSplitGoals)
	 ;  (PrunedChtree = select(SelLitNr,PrunedChpaths),
	     NrLeaves = ChildLeaves
	    )
	).


/* --------------------------- */
/* post_prune_chpaths_minlvs/6 */
/* --------------------------- */


post_prune_chpaths_minlvs(Left,Sel,Right,[],[],0).
post_prune_chpaths_minlvs(Left,Sel,Right,
	[match(ClauseNr,Children)|Rest],[match(ClauseNr,PrChildren)|PrRest],
	NrOfLeaves) :-
	copy(c(Left,Sel,Right),c(CL,CS,CR)),
	claus(ClauseNr,CS,Body),
	pp_mnf(append(Body,CR,IntGoal)),
	pp_mnf(append(CL,IntGoal,NewGoal)),
	post_prune_chtree_minlvs(NewGoal,no,Children,PrChildren,NrChild),!,
	post_prune_chpaths_minlvs(Left,Sel,Right,Rest,PrRest,NrRest),
	NrOfLeaves is NrChild + NrRest.



