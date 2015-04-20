:- module('selectionrule.dethomo-idx',['selectionrule.dethomo-idx:select_positive_literal'/5]).

:- use_package( .('../ecce_no_rt2') ).


%:- dynamic select_positive_literal/5.

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../homeomorphic').
:- use_module('../index_tools').
:- use_module('../main_functions').
:- use_module('../unfold_history').


/* An unfolding rule based on homeomorphic embedding */

'selectionrule.dethomo-idx:select_positive_literal'(Goal,TopGoalVarlist,UnfHist,NrOfSel,SelLiteral) :-
	/* Try to find determinate literals first*/
	member_nr(SelLiteral,Goal,NrOfSel),
	not(is_negative_literal(SelLiteral,Atom)),
	not(is_built_in_literal(SelLiteral)),
	not(undeterminate(Goal,NrOfSel)),
	'selectionrule.dethomo-idx:ok_to_unfold'(SelLiteral,NrOfSel,UnfHist).
'selectionrule.dethomo-idx:select_positive_literal'(Goal,TopGoalVarlist,[],NrOfSel,SelLiteral) :-
	member_nr(SelLiteral,Goal,NrOfSel),
/* allow non-leftmost undeterminate steps at top (for conjunctive PD) */
	not(is_negative_literal(SelLiteral,Atom)),
	not(is_built_in_literal(SelLiteral)).
'selectionrule.dethomo-idx:select_positive_literal'(Goal,TopGoalVarlist,UnfHist,1,SelLiteral) :-
	member_nr(SelLiteral,Goal,1),
	not(is_negative_literal(SelLiteral,Atom)),
	not(is_built_in_literal(SelLiteral)),
	indexed_varlist(TopGoalVarlist),
	not(non_indexed_unfolding(SelLiteral,TopGoalVarlist)),
	not(find_any_unimposed_variant([SelLiteral],VariantID)),
		/* stop if variant exists at the global level */
	'selectionrule.dethomo-idx:ok_to_unfold'(SelLiteral,1,UnfHist).

'selectionrule.dethomo-idx:ok_to_unfold'(SelLiteral,NrOfSel,UnfHist) :-
	not('selectionrule.dethomo-idx:embedded_covering_ancestor'(SelLiteral,NrOfSel,UnfHist)).
	/* print('selectionrule.dethomo-idx:ok_to_unfold'(SelLiteral,NrOfSel)),nl. */

'selectionrule.dethomo-idx:embedded_covering_ancestor'(SelLiteral,NrOfSel,UnfHist) :-
	covering_ancestor(NrOfSel,UnfHist,CovAncestor),
	homeomorphic_embedded(CovAncestor,SelLiteral),
	assertz(unfolding_leads_to_loop(CovAncestor)).
