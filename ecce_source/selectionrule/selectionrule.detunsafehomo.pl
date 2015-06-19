:- module('selectionrule.detunsafehomo',['selectionrule.detunsafehomo:select_positive_literal'/5]).

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
:- use_module('../main_functions').


/* An unfolding rule based on homeomorphic embedding */

'selectionrule.detunsafehomo:select_positive_literal'(Goal,TopGoalVarlist,UnfHist,NrOfSel,SelLiteral) :-
	/* Try to find determinate literals first*/
	/* without checking with covering ancestors !!! */
	member_nr(SelLiteral,Goal,NrOfSel),
	\+(is_negative_literal(SelLiteral,Atom)),
	\+(is_built_in_literal(SelLiteral)),
	\+(undeterminate(Goal,NrOfSel)).
'selectionrule.detunsafehomo:select_positive_literal'(Goal,TopGoalVarlist,UnfHist,NrOfSel,SelLiteral) :-
	member_nr(SelLiteral,Goal,NrOfSel),
	((NrOfSel=1) ; (UnfHist=[])),
/* allow non-leftmost undeterminate steps at top (for conjunctive PD) */
	\+(is_negative_literal(SelLiteral,Atom)),
	\+(is_built_in_literal(SelLiteral)),
	(\+(find_any_unimposed_variant([SelLiteral],VariantID))
	 ; (UnfHist=[])),
		/* stop if variant exists at the global level */
	'selectionrule.detunsafehomo:ok_to_unfold'(SelLiteral,NrOfSel,UnfHist).

'selectionrule.detunsafehomo:ok_to_unfold'(SelLiteral,NrOfSel,UnfHist) :-
	\+('selectionrule.detunsafehomo:embedded_covering_ancestor'(SelLiteral,NrOfSel,UnfHist)).
	/* print('selectionrule.detunsafehomo:ok_to_unfold'(SelLiteral,NrOfSel)),nl. */

'selectionrule.detunsafehomo:embedded_covering_ancestor'(SelLiteral,NrOfSel,UnfHist) :-
	covering_ancestor(NrOfSel,UnfHist,CovAncestor),
	homeomorphic_embedded(CovAncestor,SelLiteral),
	assertz(unfolding_leads_to_loop(CovAncestor)),
	debug_print(homeomorphic_embedded(CovAncestor,SelLiteral)),
	debug_nl.
