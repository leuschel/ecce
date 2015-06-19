:- module('selectionrule.det-and-once',['selectionrule.det-and-once:select_positive_literal'/5]).

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


'selectionrule.det-and-once:select_positive_literal'(Goal,TopGoalVarlist,UnfHist,NrOfSel,SelLiteral) :-
	member_nr(SelLiteral,Goal,NrOfSel),
	\+(is_negative_literal(SelLiteral,Atom)),
	\+(is_built_in_literal(SelLiteral)),
	debug_print(try(SelLiteral)),
	'selectionrule.det-and-once:ok_to_unfold'(Goal,SelLiteral,NrOfSel,UnfHist),
	debug_print(ok(SelLiteral)),debug_nl.

'selectionrule.det-and-once:ok_to_unfold'(Goal,SelLiteral,NrOfSel,[]) :- !.
'selectionrule.det-and-once:ok_to_unfold'(Goal,SelLiteral,NrOfSel,UnfHist) :-
	det_ok_to_unfold(Goal,NrOfSel,UnfHist),
	\+('selectionrule.det-and-once:embedded_covering_ancestor'(SelLiteral,NrOfSel,UnfHist)).
'selectionrule.det-and-once:ok_to_unfold'(Goal,SelLiteral,NrOfSel,UnfHist) :-
	debug_print(try_non_det(SelLiteral)),debug_nl,
	\+(more_than_one_covering_ancestor(SelLiteral,NrOfSel,UnfHist)).

det_ok_to_unfold(Goal,1,UnfHist) :-
	\+(pp_cll(contains_non_determinate_step(UnfHist))).
det_ok_to_unfold(Goal,NrOfSel,UnfHist) :-
	\+(undeterminate(Goal,NrOfSel)).

'selectionrule.det-and-once:embedded_covering_ancestor'(SelLiteral,NrOfSel,UnfHist) :-
	covering_ancestor(NrOfSel,UnfHist,CovAncestor),
	homeomorphic_embedded(CovAncestor,SelLiteral),
	debug_print(embed(CovAncestor,SelLiteral)),debug_nl.

more_than_one_covering_ancestor(SelLiteral,NrOfSel,UnfHist) :-
	covering_ancestor(NrOfSel,UnfHist,CovAncestor),
	atoms_have_same_predicate(CovAncestor,SelLiteral,_Pred),
	debug_print(not_ok(CovAncestor)),
	assertz(unfolding_leads_to_loop(CovAncestor)).
