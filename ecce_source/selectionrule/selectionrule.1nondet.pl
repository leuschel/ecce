:- module('selectionrule.1nondet',['selectionrule.1nondet:select_positive_literal'/5]).

:- use_package( .('../ecce_no_rt2') ).


%:- dynamic select_positive_literal/5.

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../homeomorphic').
:- use_module('../unfold_history').


/* A simple determinate unfolding rule w/o a depth bound */

'selectionrule.1nondet:select_positive_literal'(Goal,TopGoalVarlist,UnfHist,NrOfSel,SelLiteral) :-
	/* Try to find determinate literals */
	member_nr(SelLiteral,Goal,NrOfSel),
	\+(is_negative_literal(SelLiteral,Atom)),
	\+(is_built_in_literal(SelLiteral)),
	'selectionrule.1nondet:ok_to_unfold'(Goal,NrOfSel,UnfHist),
	\+('selectionrule.1nondet:embedded_covering_ancestor'(SelLiteral,NrOfSel,UnfHist)).

'selectionrule.1nondet:ok_to_unfold'(Goal,NrOfSel,[]) :- !.
'selectionrule.1nondet:ok_to_unfold'(Goal,1,UnfHist) :- /* ERROR IN PREVIOUS CODE ??? */
	\+(pp_cll(contains_non_determinate_step(UnfHist))),
	\+(pp_cll(goal_increasing_selection(Goal,NrOfSel))).
		/* then one will certainly profit from the extra information
			obtained by unfolding */
	/* print(ok_non_det(UnfHist)),nl. */
'selectionrule.1nondet:ok_to_unfold'(Goal,NrOfSel,UnfHist) :-
	\+(undeterminate(Goal,NrOfSel)),
	( \+(pp_cll(contains_non_determinate_step(UnfHist)))
	 ;
	 \+(pp_cll(goal_increasing_selection(Goal,NrOfSel)))
	).

'selectionrule.1nondet:embedded_covering_ancestor'(SelLiteral,NrOfSel,UnfHist) :-
	covering_ancestor(NrOfSel,UnfHist,CovAncestor),
	homeomorphic_embedded(CovAncestor,SelLiteral),
	debug_print(embed(CovAncestor,SelLiteral)),debug_nl.
