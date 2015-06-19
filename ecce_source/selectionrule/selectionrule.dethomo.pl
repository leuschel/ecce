:- module('selectionrule.dethomo',['selectionrule.dethomo:select_positive_literal'/5]).

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


/* An unfolding rule based on homeomorphic embedding */

'selectionrule.dethomo:select_positive_literal'(Goal,_TopGoalVarlist,UnfHist,NrOfSel,SelLiteral) :-
	/* Try to find determinate literals first*/
	member_nr(SelLiteral,Goal,NrOfSel),
	\+(is_negative_literal(SelLiteral,_Atom)),
	\+(is_built_in_literal(SelLiteral)),
	\+(undeterminate(Goal,NrOfSel)),
	'selectionrule.dethomo:ok_to_unfold'(SelLiteral,NrOfSel,UnfHist).
'selectionrule.dethomo:select_positive_literal'(Goal,_TopGoalVarlist,UnfHist,NrOfSel,SelLiteral) :-
	member_nr(SelLiteral,Goal,NrOfSel),
	(NrOfSel=1 ; UnfHist=[]),
/* allow non-leftmost undeterminate steps at top (for conjunctive PD) */
	\+(is_negative_literal(SelLiteral,_Atom)),
	\+(is_built_in_literal(SelLiteral)),
	(\+(find_any_unimposed_variant([SelLiteral],_VariantID))
	 ; UnfHist=[]),
		/* stop if variant exists at the global level */
	'selectionrule.dethomo:ok_to_unfold'(SelLiteral,NrOfSel,UnfHist).

'selectionrule.dethomo:ok_to_unfold'(SelLiteral,NrOfSel,UnfHist) :-
	\+('selectionrule.dethomo:embedded_covering_ancestor'(SelLiteral,NrOfSel,UnfHist)).
	/* print('selectionrule.dethomo:ok_to_unfold'(SelLiteral,NrOfSel)),nl. */
/* special case for vanilla solve (solve-test file): */
/* 'selectionrule.dethomo:ok_to_unfold'(solve(X),NrOfSel,_UnfHist) :-
	nonvar(X), print('selectionrule.dethomo:ok_to_unfold'(solve(X),NrOfSel)),nl.*/

'selectionrule.dethomo:embedded_covering_ancestor'(SelLiteral,NrOfSel,UnfHist) :-
	covering_ancestor(NrOfSel,UnfHist,CovAncestor),
	homeomorphic_embedded(CovAncestor,SelLiteral),
	assertz(unfolding_leads_to_loop(CovAncestor)),
	debug_print(homeomorphic_embedded(CovAncestor,SelLiteral)),
	debug_nl.
