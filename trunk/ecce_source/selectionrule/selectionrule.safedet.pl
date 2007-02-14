:- module('selectionrule.safedet',['selectionrule.safedet:select_positive_literal'/5]).

:- use_package( .('../ecce_no_rt2') ).


%:- dynamic select_positive_literal/5.

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../homeomorphic').
:- use_module('../unfold_history').


/* Determinate unfolding rule which checks homeomorphic embedding */

'selectionrule.safedet:select_positive_literal'(Goal,TopGoalVarlist,UnfHist,NrOfSel,SelLiteral) :-
	/* Try to find determinate literals */
	member_nr(SelLiteral,Goal,NrOfSel),
	not(is_negative_literal(SelLiteral,Atom)),
	not(is_built_in_literal(SelLiteral)),
	'selectionrule.safedet:ok_to_unfold'(SelLiteral,Goal,NrOfSel,UnfHist).

'selectionrule.safedet:ok_to_unfold'(SelLiteral,Goal,NrOfSel,[]). 
	/* allow non-determinate unfolding at top */
'selectionrule.safedet:ok_to_unfold'(SelLiteral,Goal,NrOfSel,UnfHist) :-
	not(undeterminate(Goal,NrOfSel)),
	not('selectionrule.safedet:embedded_covering_ancestor'(SelLiteral,NrOfSel,UnfHist)).

'selectionrule.safedet:embedded_covering_ancestor'(SelLiteral,NrOfSel,UnfHist) :-
	covering_ancestor(NrOfSel,UnfHist,CovAncestor),
	homeomorphic_embedded(CovAncestor,SelLiteral).
