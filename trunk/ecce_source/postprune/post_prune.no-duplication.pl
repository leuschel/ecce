:- module('post_prune.no-duplication',['post_prune.no-duplication:post_prune_chtree'/3]).

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


'post_prune.no-duplication:post_prune_chtree'(Goal,Chtree,PChtree) :-
	copy(Goal,CGoal),
	/* print_chtree(Chtree),nl, */
	'post_prune.no-duplication:post_prune_chtree'(CGoal,yes,Chtree,PChtree,NrLeaves).
	/* print_chtree(PChtree),nl. */


/* 'post_prune.no-duplication:post_prune_chtree'(G,T,C,C2,N) :-
	print('post_prune.no-duplication:post_prune_chtree'(G,C)),nl,fail. */
'post_prune.no-duplication:post_prune_chtree'(Goal,Top,success,success,0).
'post_prune.no-duplication:post_prune_chtree'(Goal,Top,stop,stop,1).
'post_prune.no-duplication:post_prune_chtree'(Goal,Top,empty,empty,0).
'post_prune.no-duplication:post_prune_chtree'(Goal,Top,remove(SelLitNr,Pred,Children),
				PrunedChtree,NrLeaves) :-
	PrunedChtree = remove(SelLitNr,Pred,PrunedChildren),
	pp_mnf(more_specific_transformation(Goal)),
	pp_mnf(split_list(Goal,SelLitNr,Left,Sel,Right)),
	pp_mnf(append(Left,Right,NewGoal)),
	'post_prune.no-duplication:post_prune_chtree'(NewGoal,no,Children,PrunedChildren,NrLeaves).
'post_prune.no-duplication:post_prune_chtree'(Goal,Top,built_in_eval(SelLitNr,BI,Children),
				PrunedChtree,NrLeaves) :-
	PrunedChtree = built_in_eval(SelLitNr,BI,PrunedChildren),
	pp_mnf(more_specific_transformation(Goal)),
	pp_mnf(split_list(Goal,SelLitNr,Left,Sel,Right)),
	(is_callable_built_in_literal(Sel) -> call_built_in(Sel) ; true),
	pp_mnf(append(Left,Right,NewGoal)),
	'post_prune.no-duplication:post_prune_chtree'(NewGoal,no,Children,PrunedChildren,NrLeaves).
'post_prune.no-duplication:post_prune_chtree'(Goal,Top,select(SelLitNr,Chpaths),PrunedChtree,NrLeaves) :-
	pp_mnf(more_specific_transformation(Goal)),
	pp_mnf(split_list(Goal,SelLitNr,Left,Sel,Right)),
	'post_prune.no-duplication:post_prune_chpaths'(Left,Sel,Right,Chpaths,
						PrunedChpaths,ChildLeaves),
	(unfolding_step_bad(ChildLeaves,Top,SelLitNr,Chpaths)
	 -> (PrunedChtree = stop, NrLeaves = 1)
	 ;  (PrunedChtree = select(SelLitNr,PrunedChpaths),
	     NrLeaves = ChildLeaves
	    )
	).

unfolding_step_bad(ChildLeaves,Top,SelLitNr,Chpaths) :-
	ChildLeaves > 1, /* more than one non-fully-resolved leaf */
	Top=no,
	SelLitNr > 1, /* non-leftmost unfolding -> might duplicate work */
	Chpaths = [Match1,Match2|RestMatches]. /* non-determinate */


/* -------------------- */
/* post_prune_chpaths/6 */
/* -------------------- */


'post_prune.no-duplication:post_prune_chpaths'(Left,Sel,Right,[],[],0).
'post_prune.no-duplication:post_prune_chpaths'(Left,Sel,Right,
	[match(ClauseNr,Children)|Rest],[match(ClauseNr,PrChildren)|PrRest],
	NrOfLeaves) :-
	copy(c(Left,Sel,Right),c(CL,CS,CR)),
	claus(ClauseNr,CS,Body),
	pp_mnf(append(Body,CR,IntGoal)),
	pp_mnf(append(CL,IntGoal,NewGoal)),
	'post_prune.no-duplication:post_prune_chtree'(NewGoal,no,Children,PrChildren,NrChild),!,
	'post_prune.no-duplication:post_prune_chpaths'(Left,Sel,Right,Rest,PrRest,NrRest),
	NrOfLeaves is NrChild + NrRest.


