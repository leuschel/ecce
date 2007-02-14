:- module('selectionrule.any-det',['selectionrule.any-det:select_positive_literal'/5]).

:- use_package( .('../ecce_no_rt2') ).


%:- dynamic select_positive_literal/5.

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../homeomorphic').
:- use_module('../unfold_history').
:- use_module('../main_functions').


/* A simple determinate unfolding rule w/o a depth bound */

'selectionrule.any-det:select_positive_literal'(Goal,_TopGoalVarlist,UnfHist,NrOfSel,SelLiteral) :-
	/* Try to find determinate literals */
	(not(find_any_unimposed_variant(Goal,_VariantID)) ; (UnfHist=[])),
		/* stop if variant exists at the global level */
	member_nr(SelLiteral,Goal,NrOfSel),
	not(is_negative_literal(SelLiteral,_Atom)),
	not(is_built_in_literal(SelLiteral)),
	'selectionrule.any-det:ok_to_unfold'(Goal,NrOfSel,UnfHist),
	not('selectionrule.any-det:embedded_covering_ancestor'(SelLiteral,NrOfSel,UnfHist)),
	debug_print(select(SelLiteral)),debug_nl.

'selectionrule.any-det:ok_to_unfold'(_Goal,_NrOfSel,[]) :- !.
'selectionrule.any-det:ok_to_unfold'(_Goal,1,UnfHist) :-
	not(pp_cll(contains_non_determinate_step(UnfHist))).
'selectionrule.any-det:ok_to_unfold'(Goal,NrOfSel,_UnfHist) :-
	not(undeterminate(Goal,NrOfSel)).

'selectionrule.any-det:embedded_covering_ancestor'(SelLiteral,NrOfSel,UnfHist) :-
	covering_ancestor(NrOfSel,UnfHist,CovAncestor),
	homeomorphic_embedded(CovAncestor,SelLiteral),
	debug_print(embed(CovAncestor,SelLiteral)),debug_nl.
