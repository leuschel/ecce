:- module('selectionrule.homo',['selectionrule.homo:select_positive_literal'/5]).

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

'selectionrule.homo:select_positive_literal'(Goal,TopGoalVarlist,UnfHist,NrOfSel,SelLiteral) :-
	'selectionrule.homo:goal_can_be_unfolded'(Goal,UnfHist),!,
	'selectionrule.homo:select_positive_literal2'(Goal,UnfHist,NrOfSel,SelLiteral),
	'selectionrule.homo:ok_to_unfold'(SelLiteral,NrOfSel,UnfHist),
	/* print(ok(SelLiteral)),print(in(Goal)),nl,
	print(UnfHist),nl, */ !.

'selectionrule.homo:select_positive_literal2'(Goal,UnfHist,NrOfSel,SelLiteral) :-
	member_nr(SelLiteral,Goal,NrOfSel),
	\+(is_negative_literal(SelLiteral,Atom)),
	\+(is_built_in_literal(SelLiteral)),
	\+(undeterminate(Goal,NrOfSel)).
		/* first look for determinate unfolding steps */
'selectionrule.homo:select_positive_literal2'(Goal,UnfHist,NrOfSel,SelLiteral) :-
	member_nr(SelLiteral,Goal,NrOfSel),
	\+(is_negative_literal(SelLiteral,Atom)),
	\+(is_built_in_literal(SelLiteral)),
	((UnfHist=[]) ; reduces_search_space(Goal,NrOfSel)).

'selectionrule.homo:goal_can_be_unfolded'(_Goal,[]) :- !.
'selectionrule.homo:goal_can_be_unfolded'([_Atom],_UnfHist) :- !,fail.
	/* don't unfold on the basis that no loss of precision due
		to splitting */
'selectionrule.homo:goal_can_be_unfolded'(Goal,UnfHist) :-
	partition_goal(Goal,SplitGoals),
	member(split_goal(SGoal,Pos),SplitGoals),
	/* SGoal \== [Atom], */
	\+(find_unimposed_instance(SGoal,VariantID)),
	debug_print(not_variant(Goal)),debug_nl.

'selectionrule.homo:ok_to_unfold'(SelLiteral,NrOfSel,UnfHist) :-
	\+('selectionrule.homo:embedded_covering_ancestor'(SelLiteral,NrOfSel,UnfHist)).

'selectionrule.homo:embedded_covering_ancestor'(SelLiteral,NrOfSel,UnfHist) :-
	covering_ancestor(NrOfSel,UnfHist,CovAncestor),
	(homeomorphic_embedded(CovAncestor,SelLiteral)
	 -> true
	 ;  (debug_print(not_embeddeding),debug_nl,
		debug_print(SelLiteral),debug_nl,debug_print(CovAncestor),
		debug_nl,fail )
	),
	assertz(unfolding_leads_to_loop(CovAncestor)).
	/* print(homo_embed(CovAncestor,SelLiteral)),nl. */
