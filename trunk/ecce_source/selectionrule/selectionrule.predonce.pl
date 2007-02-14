:- module('selectionrule.predonce',['selectionrule.predonce:select_positive_literal'/5]).

:- use_package( .('../ecce_no_rt2') ).


%:- dynamic select_positive_literal/5.

:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../main_functions').
:- use_module('../unfold_history').


'selectionrule.predonce:select_positive_literal'(Goal,TopGoalVarlist,UnfHist,NrOfSel,SelLiteral) :-
	'selectionrule.predonce:goal_can_be_unfolded'(Goal,UnfHist),
	member_nr(SelLiteral,Goal,NrOfSel),
	not(is_negative_literal(SelLiteral,Atom)),
	not(is_built_in_literal(SelLiteral)),
	debug_print(try(SelLiteral)),
	'selectionrule.predonce:ok_to_unfold'(SelLiteral,NrOfSel,UnfHist),
	debug_print(ok(SelLiteral)),debug_nl.

'selectionrule.predonce:goal_can_be_unfolded'(Goal,[]).
'selectionrule.predonce:goal_can_be_unfolded'(Goal,UnfHist) :-
	partition_goal(Goal,SplitGoals),
	member(split_goal(SGoal,Pos),SplitGoals),
	not(find_any_unimposed_variant(SGoal,VariantID)),
	debug_print(not_variant(Goal)),debug_nl.

'selectionrule.predonce:ok_to_unfold'(SelLiteral,NrOfSel,UnfHist) :-
	not('selectionrule.predonce:embedded_covering_ancestor'(SelLiteral,NrOfSel,UnfHist)).

'selectionrule.predonce:embedded_covering_ancestor'(SelLiteral,NrOfSel,UnfHist) :-
	covering_ancestor(NrOfSel,UnfHist,CovAncestor),
	atoms_have_same_predicate(CovAncestor,SelLiteral,_Pred),
	debug_print(not_ok(CovAncestor)),
	assertz(unfolding_leads_to_loop(CovAncestor)).
	/* print(homo_embed(CovAncestor,SelLiteral)),nl. */
