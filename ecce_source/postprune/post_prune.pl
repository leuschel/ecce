:- module(post_prune,[post_prune_chtree/3]).

/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,96,97,98  */
/* --------------------------------------------- */

/* TO GET THE LATEST VERSION OF THE ECCE SYSTEM GO TO:
	http://www.cs.kuleuven.ac.be/~michael */
/* BUG REPORTS, QUESTIONS AND COMMENTS CAN BE SENT TO:
	michael@cs.kuleuven.ac.be */

/* file: parameters.pro */

:- use_module('../dynpreds').
%:- dynamic post_prune_chtree/3.
:- multifile available_options/4.


:- use_module('post_prune.detidx.minleaves.pl').
:- use_module('post_prune.minleaves.detidx.pl').
:- use_module('post_prune.det.pl').
:- use_module('post_prune.no-duplication.pl').
:- use_module('post_prune.det-indexed.pl').
:- use_module('post_prune.minleaves.pl').
:- use_module('post_prune.min-nonv-leaves.pl').
:- use_module('post_prune.none.pl').
:- use_module('post_prune.mixtus.pl').

post_prune_chtree(G,C,P) :- 
   get_current_parameter_value(postprune,Z),
   'post_prune:dispatch'(Z,G,C,P).

'post_prune:dispatch'(98 ,G,C,P) :- 'post_prune.detidx.minleaves:post_prune_chtree'(G,C,P).
'post_prune:dispatch'(99 ,G,C,P) :- 'post_prune.minleaves.detidx:post_prune_chtree'(G,C,P).
'post_prune:dispatch'(100,G,C,P) :- 'post_prune.det:post_prune_chtree'(G,C,P).
'post_prune:dispatch'(101,G,C,P) :- 'post_prune.no-duplication:post_prune_chtree'(G,C,P).
'post_prune:dispatch'(105,G,C,P) :- 'post_prune.det-indexed:post_prune_chtree'(G,C,P).
'post_prune:dispatch'(109,G,C,P) :- 'post_prune.minleaves:post_prune_chtree'(G,C,P).
'post_prune:dispatch'(111,G,C,P) :- 'post_prune.min-nonv-leaves:post_prune_chtree'(G,C,P).
'post_prune:dispatch'(110,G,C,P) :- 'post_prune.none:post_prune_chtree'(G,C,P).
'post_prune:dispatch'(120,G,C,P) :- 'post_prune.mixtus:post_prune_chtree'(G,C,P).


/* =================== */
/*     POST PRUNING    */
/* =================== */
available_options(postprune,98,
	'postprune/post_prune.detidx.minleaves.pl',
	'combination of (i) indexed det. and (m) minimise leaf atoms  pruning') :- 
	user_expert(yes).
available_options(postprune,99,
	'postprune/post_prune.minleaves.detidx.pl',
	'Combination of (m) minimise leaf atoms and (i) indexed det.  pruning') :- 
	user_expert(yes).
available_options(postprune,100,
	'postprune/post_prune.det.pl',
	'cut back to Determinate tree').
available_options(postprune,101,
	'postprune/post_prune.no-duplication.pl',
	'prune such that there is no duplication of work (except maybe for unifications)').
available_options(postprune,105,
	'postprune/post_prune.det-indexed.pl',
	'cut back to Indexed determinate tree').
available_options(postprune,109,
	'postprune/post_prune.minleaves.pl',
	'minimise number of descending leaf nodes').
available_options(postprune,111,
	'postprune/post_prune.min-nonv-leaves.pl',
	'minimise number of non-covered (i.e. new) descending leaf atoms').
available_options(postprune,110,
	'postprune/post_prune.none.pl',
	'No post unfolding pruning').
available_options(postprune,120,
	'postprune/post_prune.mixtus.pl',
	'miXtus like (maximum 10 number of clauses for an unfolding step)').
