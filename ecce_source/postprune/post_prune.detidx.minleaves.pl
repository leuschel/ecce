:- module('post_prune.detidx.minleaves',['post_prune.detidx.minleaves:post_prune_chtree'/3]).

:- use_package( .('../ecce_no_rt2') ).

%:- dynamic post_prune_chtree/3.

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../main_functions').
:- use_module('../index_tools').

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

:- use_module( 'post_prune.common.pl' ).

:- dynamic 'post_prune.detidx.minleaves:ppc_already_unfolded'/0.
'post_prune.detidx.minleaves:ppc_already_unfolded'.

'post_prune.detidx.minleaves:reset_ppc_already_unfolded' :-
	retract('post_prune.detidx.minleaves:ppc_already_unfolded'),fail.
'post_prune.detidx.minleaves:reset_ppc_already_unfolded'.



'post_prune.detidx.minleaves:post_prune_chtree'(Goal,Chtree,PPChtree) :-
	/* print_chtree(Chtree),nl, */
	'post_prune.detidx.minleaves:reset_ppc_already_unfolded',
	copy(Goal,CGoal2),
	varlist(CGoal2,Varlist),
	'post_prune.detidx.minleaves:post_prune_chtree_detidx'(CGoal2,Varlist,Chtree,PChtree),
	copy(Goal,CGoal),
	post_prune_chtree_minlvs(CGoal,yes,PChtree,PPChtree,NrLeaves).






/* -------------------------- DET-INDEX PART ----------------------------- */


/* 'post_prune.detidx.minleaves:post_prune_chtree_detidx'(G,V,C,OC) :-
	print(call_post_prune(G,V,C)),nl,fail. */
'post_prune.detidx.minleaves:post_prune_chtree_detidx'(Goal,Varlist,success,success).
'post_prune.detidx.minleaves:post_prune_chtree_detidx'(Goal,Varlist,stop,stop).
'post_prune.detidx.minleaves:post_prune_chtree_detidx'(Goal,Varlist,empty,empty).
'post_prune.detidx.minleaves:post_prune_chtree_detidx'(Goal,Varlist,remove(SelLitNr,Pred,Children),
				PrunedChtree) :-
	PrunedChtree = remove(SelLitNr,Pred,PrunedChildren),
	pp_mnf(more_specific_transformation(Goal)),
	pp_mnf(split_list(Goal,SelLitNr,Left,Sel,Right)),
	pp_mnf(append(Left,Right,NewGoal)),
	'post_prune.detidx.minleaves:post_prune_chtree_detidx'(NewGoal,Varlist,Children,PrunedChildren).
'post_prune.detidx.minleaves:post_prune_chtree_detidx'(Goal,Varlist,built_in_eval(SelLitNr,BI,Children),
				PrunedChtree) :-
	PrunedChtree = built_in_eval(SelLitNr,BI,PrunedChildren),
	pp_mnf(more_specific_transformation(Goal)),
	pp_mnf(split_list(Goal,SelLitNr,Left,Sel,Right)),
	(is_callable_built_in_literal(Sel) -> call_built_in(Sel) ; true),
	pp_mnf(append(Left,Right,NewGoal)),
	'post_prune.detidx.minleaves:post_prune_chtree_detidx'(NewGoal,Varlist,Children,PrunedChildren).
'post_prune.detidx.minleaves:post_prune_chtree_detidx'(Goal,Varlist,select(SelLitNr,Chpaths),PrunedChtree) :-
	pp_mnf(more_specific_transformation(Goal)),
	pp_mnf(split_list(Goal,SelLitNr,Left,Sel,Right)),
	(('post_prune.detidx.minleaves:prune_this_step'(Sel,Varlist,Chpaths),
	  'post_prune.detidx.minleaves:ppc_already_unfolded')
	 -> (PrunedChtree = stop)
	 ;  (PrunedChtree = select(SelLitNr,PrunedChpaths),
	     ('post_prune.detidx.minleaves:ppc_already_unfolded' -> true ; assert('post_prune.detidx.minleaves:ppc_already_unfolded') ),
	     'post_prune.detidx.minleaves:post_prune_chpaths_detidx'(Left,Sel,Right,Varlist,Chpaths,
							PrunedChpaths)
	    )
	).

'post_prune.detidx.minleaves:prune_this_step'(Sel,Varlist,[Match1,Match2|RM]) :-
	\+(indexed_varlist(Varlist)).
'post_prune.detidx.minleaves:prune_this_step'(Sel,Varlist,[Match1,Match2|RM]) :-
	non_indexed_unfolding(Sel,Varlist).
'post_prune.detidx.minleaves:prune_this_step'(Sel,Varlist,Chpaths) :-
	unfolding_leads_to_loop(Sel).




/* -------------------- */
/* post_prune_chpaths/6 */
/* -------------------- */

'post_prune.detidx.minleaves:post_prune_chpaths_detidx'(Left,Sel,Right,Varlist,[],[]).
'post_prune.detidx.minleaves:post_prune_chpaths_detidx'(Left,Sel,Right,Varlist,
	[match(ClauseNr,Children)|Rest],[match(ClauseNr,PrChildren)|PrRest]) :-
	copy(c(Left,Sel,Right,Varlist),c(CL,CS,CR,CV)),
	claus(ClauseNr,CS,Body),
	pp_mnf(append(Body,CR,IntGoal)),
	pp_mnf(append(CL,IntGoal,NewGoal)),
	'post_prune.detidx.minleaves:post_prune_chtree_detidx'(NewGoal,CV,Children,PrChildren),
	'post_prune.detidx.minleaves:post_prune_chpaths_detidx'(Left,Sel,Right,Varlist,Rest,PrRest).


