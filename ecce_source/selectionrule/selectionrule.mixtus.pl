:- module('selectionrule.mixtus',['selectionrule.mixtus:select_positive_literal'/5]).

:- use_package( .('../ecce_no_rt2') ).


%:- dynamic select_positive_literal/5.

:- set_prolog_flag(multi_arity_warnings,off).
%:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../main_functions').
:- use_module('../unfold_history').
:- use_module('../homeomorphic').
:- use_module('../static_dynamic_functors').

:- use_module( 'selectionrule.common.pl' ).

/* An mixtus like unfolding rule */

'selectionrule.mixtus:select_positive_literal'(Goal,TopGoalVarlist,UnfHist,NrOfSel,SelLiteral) :-
	get_sel_literal(Goal,SelLiteral,NrOfSel,UnfHist),
/* allow non-leftmost undeterminate steps at top (for conjunctive PD) */
	\+(is_negative_literal(SelLiteral,Atom)),
	\+(is_built_in_literal(SelLiteral)),
	(\+(find_any_unimposed_variant([SelLiteral],VariantID))
	 ; (UnfHist=[])),
		/* stop if variant exists at the global level */
	(\+(unfolding_leads_to_loop(SelLiteral)) ; (UnfHist=[])),
	'selectionrule.mixtus:ok_to_unfold'(SelLiteral,NrOfSel,UnfHist).

'selectionrule.mixtus:ok_to_unfold'(SelLiteral,NrOfSel,UnfHist) :-
	\+('selectionrule.mixtus:loop_prevention'(SelLiteral,NrOfSel,UnfHist)),
	debug_print('selectionrule.mixtus:ok_to_unfold'(SelLiteral,NrOfSel)),debug_nl.


'selectionrule.mixtus:loop_prevention'(SelLiteral,NrOfSel,UnfHist) :-
	findall(mayloop,
		'selectionrule.mixtus:may_loop_covering_ancestor'(SelLiteral,NrOfSel,UnfHist),
		Sols),
	length(Sols,NrML),
	NrML >= 2,  /* ===> max_rec = 2 */
	/* may_loop_covering_ancestor(SelLiteral,NrOfSel,UnfHist),!, */
	debug_print('selectionrule.mixtus:loop_prevention'(SelLiteral,NrOfSel)),debug_nl.

'selectionrule.mixtus:may_loop_covering_ancestor'(SelLiteral,NrOfSel,UnfHist) :-
	covering_ancestor(NrOfSel,UnfHist,CovAncestor),
	may_loop(CovAncestor,SelLiteral),
	debug_print('selectionrule.mixtus:mayloop'(CovAncestor,SelLiteral)),debug_nl,
	assertz(unfolding_leads_to_loop(CovAncestor)).

/*
'selectionrule.mixtus:may_loop'(T,S) :-
	'selectionrule.mixtus:may_loop2'(T,S,2). % max_depth = 2 

'selectionrule.mixtus:may_loop2'(T,S,Depth) :-
	'selectionrule.mixtus:stop_may_loop'(T,S,Depth),!,
	mixtus_term_size(T,TS),
	mixtus_term_size(S,SS),
	TS =< SS.
'selectionrule.mixtus:may_loop2'(T,S,Depth) :-
	nonvar(T),
	nonvar(S),
	atoms_have_same_predicate(T,S,Predicate),
	D1 is Depth - 1,
	T =.. [Pred|TA],
	S =.. [Pred|TS],
	'selectionrule.mixtus:all_may_loop2'(TA,TS,D1).

'selectionrule.mixtus:stop_may_loop'(T,S,Depth) :- Depth < 1.
'selectionrule.mixtus:stop_may_loop'(T,S,Depth) :- var(S),!.
'selectionrule.mixtus:stop_may_loop'(T,S,Depth) :- dynamic_term(S),\+(number(S)).
'selectionrule.mixtus:stop_may_loop'(T,S,Depth) :- 'selectionrule.mixtus:inf_number'(S).

'selectionrule.mixtus:inf_number'(T) :-
	number(T),
	AT is abs(T),
	AT >= 7. % maxfinite = 7

'selectionrule.mixtus:all_may_loop2'([],[],D).
'selectionrule.mixtus:all_may_loop2'([H|T],[SH|ST],D) :-
	'selectionrule.mixtus:may_loop2'(H,SH,D),
	'selectionrule.mixtus:all_may_loop2'(T,ST,D).

*/
