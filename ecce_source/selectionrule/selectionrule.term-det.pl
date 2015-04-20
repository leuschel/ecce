:- module('selectionrule.term-det',['selectionrule.term-det:select_positive_literal'/5]).

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

'selectionrule.term-det:select_positive_literal'(Goal,TopGoalVarlist,UnfHist,NrOfSel,SelLiteral) :-
	/* Try to find determinate literals */
	member_nr(SelLiteral,Goal,NrOfSel),
	not(is_negative_literal(SelLiteral,Atom)),
	not(is_built_in_literal(SelLiteral)),
	'selectionrule.term-det:ok_to_unfold'(Goal,NrOfSel,UnfHist),
	not('selectionrule.term-det:embedded_covering_ancestor'(SelLiteral,NrOfSel,UnfHist)).
	%print(select(Goal,NrOfSel,SelLiteral)),nl.

'selectionrule.term-det:ok_to_unfold'(Goal,NrOfSel,[]) :- !.
'selectionrule.term-det:ok_to_unfold'(Goal,1,UnfHist) :- 
	not(undeterminate(Goal,NrOfSel)) ;
	not(pp_cll(contains_non_determinate_step(UnfHist))).
	

'selectionrule.term-det:embedded_covering_ancestor'(SelLiteral,NrOfSel,UnfHist) :-
	covering_ancestor(NrOfSel,UnfHist,CovAncestor),
	homeomorphic_embedded(CovAncestor,SelLiteral),
	debug_print(embed(CovAncestor,SelLiteral)),debug_nl.
