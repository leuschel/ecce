:- module('selectionrule.homo-leftmost',['selectionrule.homo-leftmost:select_positive_literal'/5]).

:- use_package( .('../ecce_no_rt2') ).


/* preserves termination: useful for termination analysis */


%:- dynamic select_positive_literal/5.

:- set_prolog_flag(multi_arity_warnings,off).
%:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../homeomorphic').
:- use_module('../main_functions').
:- use_module('../unfold_history').

/* An unfolding rule based on homeomorphic embedding */

'selectionrule.homo-leftmost:select_positive_literal'(Goal,_TopGoalVarlist,UnfHist,NrOfSel,SelLiteral) :-
	'selectionrule.homo-leftmost:goal_can_be_unfolded'(Goal,UnfHist),!,
	'selectionrule.homo-leftmost:select_positive_literal2'(Goal,UnfHist,NrOfSel,SelLiteral),
	'selectionrule.homo-leftmost:ok_to_unfold'(SelLiteral,NrOfSel,UnfHist),
	/* 
	((SelLiteral = unfold(X,_,_),\+(X=[]))
 	 -> (print(ok(SelLiteral)),nl,print(in(Goal)),nl,nl,
	     debug_print(UnfHist),nl,nl)
	 ;  true
	), */ !.
	/* print(select(NrOfSel)),
	print(literal(SelLiteral)),nl,
	print(goal(Goal)),nl. */


'selectionrule.homo-leftmost:select_positive_literal2'(Goal,_UnfHist,1,SelLiteral) :-
    %print(sel(Goal,1,SelLiteral)),
	member_nr(SelLiteral,Goal,1), /* only allow selection of leftmost call */
	\+(is_negative_literal(SelLiteral,_Atom)),
	\+(is_built_in_literal(SelLiteral)).

'selectionrule.homo-leftmost:goal_can_be_unfolded'(_Goal,[]) :- !.
'selectionrule.homo-leftmost:goal_can_be_unfolded'(Goal,_UnfHist) :-
	partition_goal(Goal,SplitGoals),
	member(split_goal(SGoal,_Pos),SplitGoals),
	/* SGoal \== [Atom], */
	\+(find_unimposed_instance(SGoal,_VariantID)),
	debug_print(not_variant(Goal)),debug_nl.

'selectionrule.homo-leftmost:ok_to_unfold'(SelLiteral,NrOfSel,UnfHist) :-
	\+('selectionrule.homo-leftmost:embedded_covering_ancestor'(SelLiteral,NrOfSel,UnfHist)).

'selectionrule.homo-leftmost:embedded_covering_ancestor'(SelLiteral,NrOfSel,UnfHist) :-
	covering_ancestor(NrOfSel,UnfHist,CovAncestor),
	(homeomorphic_embedded(CovAncestor,SelLiteral)
	 -> true
	 ;  (/*SelLiteral = unfold(XU,_,_),\+(XU=[]),
		print(not_embeddeding),nl,
		print(SelLiteral),nl,print(CovAncestor),
		nl,nl,*/fail )
	),
	assertz(unfolding_leads_to_loop(CovAncestor)).
	/* print(homo_embed(CovAncestor,SelLiteral)),nl. */
