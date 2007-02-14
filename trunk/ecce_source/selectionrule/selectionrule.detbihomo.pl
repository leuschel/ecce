:- module('selectionrule.detbihomo',['selectionrule.detbihomo:select_positive_literal'/5]).

:- use_package( .('../ecce_no_rt2') ).


%:- dynamic select_positive_literal/5.

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../homeomorphic').
:- use_module('../main_functions').
:- use_module('../unfold_history').

:- use_module( 'selectionrule.common.pl' ).


/* An unfolding rule based on homeomorphic embedding */


/* allow non-leftmost undeterminate steps only at top or after built-ins */


'selectionrule.detbihomo:select_positive_literal'(Goal,TopGoalVarlist,UnfHist,NrOfSel,SelLiteral) :-
	/* Try to find determinate literals first*/
	member_nr(SelLiteral,Goal,NrOfSel),
	not(is_negative_literal(SelLiteral,Atom)),
	not(is_built_in_literal(SelLiteral)),
	not(undeterminate(Goal,NrOfSel)),
	'selectionrule.detbihomo:ok_to_unfold'(SelLiteral,NrOfSel,UnfHist).
'selectionrule.detbihomo:select_positive_literal'(Goal,TopGoalVarlist,UnfHist,NrOfSel,SelLiteral) :-
	get_sel_literal(Goal,SelLiteral,NrOfSel,UnfHist),
	not(is_negative_literal(SelLiteral,Atom)),
	not(is_built_in_literal(SelLiteral)),
	((UnfHist \== [], find_any_unimposed_variant([SelLiteral],VariantID))
	 -> (/*print(stop_unfolding_variant(VariantID,SelLiteral)),nl,*/
	     fail)
	 ;  (true)
	),
		/* stop if variant exists at the global level */
	'selectionrule.detbihomo:ok_to_unfold'(SelLiteral,NrOfSel,UnfHist).

'selectionrule.detbihomo:ok_to_unfold'(SelLiteral,NrOfSel,UnfHist) :-
	not('selectionrule.detbihomo:embedded_covering_ancestor'(SelLiteral,NrOfSel,UnfHist)).
	/* print('selectionrule.detbihomo:ok_to_unfold'(SelLiteral,NrOfSel)),nl. */

'selectionrule.detbihomo:embedded_covering_ancestor'(SelLiteral,NrOfSel,UnfHist) :-
	covering_ancestor(NrOfSel,UnfHist,CovAncestor),
	homeomorphic_embedded(CovAncestor,SelLiteral),
	assertz(unfolding_leads_to_loop(CovAncestor)).
	/* print(embedded_in(SelLiteral,CovAncestor)),nl. */


