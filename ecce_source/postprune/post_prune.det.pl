:- module('post_prune.det',['post_prune.det:post_prune_chtree'/3]).

%:- dynamic post_prune_chtree/3.

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../main_functions').

'post_prune.det:post_prune_chtree'(_Goal,Chtree,PChtree) :-
	'post_prune.det:post_prune_chtree'(Chtree,PChtree).


'post_prune.det:post_prune_chtree'(success,success).
'post_prune.det:post_prune_chtree'(stop,stop).
'post_prune.det:post_prune_chtree'(empty,empty).
'post_prune.det:post_prune_chtree'(remove(SelLitNr,Pred,Children),PrunedChtree) :-
	PrunedChtree = remove(SelLitNr,Pred,PrunedChildren),
	post_prune_chtree_nondet(Children,PrunedChildren).
'post_prune.det:post_prune_chtree'(built_in_eval(SelLitNr,BI,Children),PrunedChtree) :-
	PrunedChtree = built_in_eval(SelLitNr,BI,PrunedChildren),
	post_prune_chtree_nondet(Children,PrunedChildren).
'post_prune.det:post_prune_chtree'(select(LiteralNr,Chpaths),PrunedChtree) :-
	PrunedChtree = select(LiteralNr,PrunedChpaths),
	post_prune_chpaths_nondet(Chpaths,PrunedChpaths).


/* --------------------------- */
/* post_prune_chpaths_nondet/2 */
/* --------------------------- */


post_prune_chpaths_nondet([],[]).
post_prune_chpaths_nondet([match(ClauseNr,Children)|Rest],[PrChpath|PrRest]) :-
	PrChpath = match(ClauseNr,PrChildren),
	post_prune_chtree_nondet(Children,PrChildren),
	post_prune_chpaths_nondet(Rest,PrRest).


/* -------------------------- */
/* post_prune_chtree_nondet/2 */
/* -------------------------- */

/* PRUNE ANY NON-DETERMINATE STEPS */

post_prune_chtree_nondet(success,success).
post_prune_chtree_nondet(stop,stop).
post_prune_chtree_nondet(empty,empty).
post_prune_chtree_nondet(remove(SelLitNr,Pred,Children),PrunedChtree) :-
	PrunedChtree = remove(SelLitNr,Pred,PrunedChildren),
	post_prune_chtree_nondet(Children,PrunedChildren).
post_prune_chtree_nondet(built_in_eval(SelLitNr,BI,Children),PrunedChtree) :-
	PrunedChtree = built_in_eval(SelLitNr,BI,PrunedChildren),
	post_prune_chtree_nondet(Children,PrunedChildren).
post_prune_chtree_nondet(select(LiteralNr,Chpaths),PrunedChtree) :-
	((Chpaths=[Path1,Path2|Rest])
	-> (/* non determinate -> prune */
	    PrunedChtree = stop
	   )
	;  (/* ok determinate */
	    PrunedChtree = select(LiteralNr,PrunedChpaths),
	    post_prune_chpaths_nondet(Chpaths,PrunedChpaths)
	   )
	).
