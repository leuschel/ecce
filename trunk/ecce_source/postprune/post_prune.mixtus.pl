:- module('post_prune.mixtus',['post_prune.mixtus:post_prune_chtree'/3]).

:- use_package( .('../ecce_no_rt2') ).

%:- dynamic post_prune_chtree/3.


:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../main_functions').

:- use_module('../abstract/abstract').
:- use_module('../more_specific/more_specific').
:- use_module('../neg_solve/neg_solve').
:- use_module('../partition/partition').
:- use_module('../chtree_tools').
:- use_module('../unfold_history').
:- use_module('../global_tree').
:- use_module('../code_generator').
:- use_module('../modes').
:- use_module('../raf_analysis').


'post_prune.mixtus:post_prune_chtree'(Goal,Chtree,PChtree) :-
	copy(Goal,CGoal),
	/* print_chtree(Chtree),nl, */
	'post_prune.mixtus:post_prune_chtree'(CGoal,yes,Chtree,PChtree,NrLeaves).
	/* print_chtree(PChtree),nl. */




/* 'post_prune.mixtus:post_prune_chtree'(G,T,C,OC,NrLvs) :-
	print(call_post_prune(G,T,C)),nl,fail. */
'post_prune.mixtus:post_prune_chtree'(Goal,Top,success,success,0).
'post_prune.mixtus:post_prune_chtree'(Goal,Top,stop,stop,1).
'post_prune.mixtus:post_prune_chtree'(Goal,Top,empty,empty,0).
'post_prune.mixtus:post_prune_chtree'(Goal,Top,remove(SelLitNr,Pred,Children),
				PrunedChtree,NrClauses) :-
	PrunedChtree = remove(SelLitNr,Pred,PrunedChildren),
	pp_mnf(more_specific_transformation(Goal)),
	pp_mnf(split_list(Goal,SelLitNr,Left,Sel,Right)),
	pp_mnf(append(Left,Right,NewGoal)),
	'post_prune.mixtus:post_prune_chtree'(NewGoal,no,Children,PrunedChildren,NrClauses).
'post_prune.mixtus:post_prune_chtree'(Goal,Top,built_in_eval(SelLitNr,BI,Children),
				PrunedChtree,NrClauses) :-
	PrunedChtree = built_in_eval(SelLitNr,BI,PrunedChildren),
	pp_mnf(more_specific_transformation(Goal)),
	pp_mnf(split_list(Goal,SelLitNr,Left,Sel,Right)),
	(is_callable_built_in_literal(Sel) -> call_built_in(Sel) ; true),
	pp_mnf(append(Left,Right,NewGoal)),
	'post_prune.mixtus:post_prune_chtree'(NewGoal,no,Children,PrunedChildren,NrClauses).
'post_prune.mixtus:post_prune_chtree'(Goal,Top,select(SelLitNr,Chpaths),PrunedChtree,NrClauses) :-
	pp_mnf(more_specific_transformation(Goal)),
	pp_mnf(split_list(Goal,SelLitNr,Left,Sel,Right)),
	((unfolding_leads_to_loop(Sel),Top=no)
	-> (PrunedChtree = stop, NrClauses = 1)
	;  (post_prune_chpaths(Left,Sel,Right,Chpaths,
						PrunedChpaths,ChildClauses),
	    ((ChildClauses > 10,  /* maxnondeterm = 10 */
	      Top=no)
	     -> (PrunedChtree = stop, NrClauses = 1)
	     ;  (PrunedChtree = select(SelLitNr,PrunedChpaths),
	         NrClauses = ChildClauses
	        )
	    )
	   )
	).


/* -------------------- */
/* post_prune_chpaths/6 */
/* -------------------- */


post_prune_chpaths(Left,Sel,Right,[],[],0).
post_prune_chpaths(Left,Sel,Right,
	[match(ClauseNr,Children)|Rest],[match(ClauseNr,PrChildren)|PrRest],
	NrClauses) :-
	copy(c(Left,Sel,Right),c(CL,CS,CR)),
	claus(ClauseNr,CS,Body),
	pp_mnf(append(Body,CR,IntGoal)),
	pp_mnf(append(CL,IntGoal,NewGoal)),
	'post_prune.mixtus:post_prune_chtree'(NewGoal,no,Children,PrChildren,NrChild),!,
	post_prune_chpaths(Left,Sel,Right,Rest,PrRest,NrRest),
	NrClauses is NrChild + NrRest.


