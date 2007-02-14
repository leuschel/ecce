:- module('selectionrule.mixtus-det',['selectionrule.mixtus-det:select_positive_literal'/5]).


:- use_package( .('../ecce_no_rt2') ).

%:- dynamic select_positive_literal/5.

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../unfold_history').
:- use_module('../main_functions').
:- use_module('../homeomorphic').
:- use_module('../static_dynamic_functors').

:- use_module( 'selectionrule.common.pl' ).


/* An unfolding rule based on homeomorphic embedding */

'selectionrule.mixtus-det:select_positive_literal'(Goal,TopGoalVarlist,UnfHist,NrOfSel,SelLiteral) :-
	member_nr(SelLiteral,Goal,NrOfSel),
	((NrOfSel=1) ; (UnfHist=[])),
/* allow non-leftmost undeterminate steps at top (for conjunctive PD) */
	not(is_negative_literal(SelLiteral,Atom)),
	not(is_built_in_literal(SelLiteral)),
	(not(find_any_unimposed_variant([SelLiteral],VariantID))
	 ; (UnfHist=[])),
		/* stop if variant exists at the global level */
	(not(unfolding_leads_to_loop(SelLiteral)) ; (UnfHist=[])),
	'selectionrule.mixtus-det:ok_to_unfold'(SelLiteral,NrOfSel,UnfHist),!.
'selectionrule.mixtus-det:select_positive_literal'(Goal,TopGoalVarlist,UnfHist,NrOfSel,SelLiteral) :-
	/* Try to find determinate literals */
	member_nr(SelLiteral,Goal,NrOfSel),
	NrOfSel > 1,
	not(is_negative_literal(SelLiteral,Atom)),
	not(is_built_in_literal(SelLiteral)),
	not(undeterminate(Goal,NrOfSel)),
	not(unfolding_leads_to_loop(SelLiteral)),
	'selectionrule.mixtus-det:ok_to_unfold'(SelLiteral,NrOfSel,UnfHist).

'selectionrule.mixtus-det:ok_to_unfold'(SelLiteral,NrOfSel,UnfHist) :-
	not(loop_prevention(SelLiteral,NrOfSel,UnfHist)),
	debug_print('selectionrule.mixtus-det:ok_to_unfold'(SelLiteral,NrOfSel)),debug_nl.


loop_prevention(SelLiteral,NrOfSel,UnfHist) :-
	/* findall(mayloop,
		may_loop_covering_ancestor(SelLiteral,NrOfSel,UnfHist),
		Sols),
	length(Sols,NrML),
	NrML >= 2,   ===> max_rec = 2 */
	may_loop_covering_ancestor(SelLiteral,NrOfSel,UnfHist),!,
	debug_print(loop_prevention(SelLiteral,NrOfSel)),debug_nl.

may_loop_covering_ancestor(SelLiteral,NrOfSel,UnfHist) :-
	covering_ancestor(NrOfSel,UnfHist,CovAncestor),
	may_loop(CovAncestor,SelLiteral),
	assertz(unfolding_leads_to_loop(CovAncestor)).


