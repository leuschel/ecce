:- module('selectionrule.homo-pure',['selectionrule.homo-pure:select_positive_literal'/5]).

:- use_package( .('../ecce_no_rt2') ).


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

'selectionrule.homo-pure:select_positive_literal'(Goal,_TopGoalVarlist,UnfHist,NrOfSel,SelLiteral) :-
	'selectionrule.homo-pure:goal_can_be_unfolded'(Goal,UnfHist),!,
	'selectionrule.homo-pure:select_positive_literal2'(Goal,UnfHist,NrOfSel,SelLiteral),
	'selectionrule.homo-pure:ok_to_unfold'(SelLiteral,NrOfSel,UnfHist),
	/* 
	((SelLiteral = unfold(X,_,_),not(X=[]))
 	 -> (print(ok(SelLiteral)),nl,print(in(Goal)),nl,nl,
	     debug_print(UnfHist),nl,nl)
	 ;  true
	), */ !.
	/* print(select(NrOfSel)),
	print(literal(SelLiteral)),nl,
	print(goal(Goal)),nl. */

'selectionrule.homo-pure:select_positive_literal2'(Goal,_UnfHist,NrOfSel,SelLiteral) :-
	member_nr(SelLiteral,Goal,NrOfSel),
	not(is_negative_literal(SelLiteral,_Atom)),
	not(is_built_in_literal(SelLiteral)),
	not(undeterminate(Goal,NrOfSel)).
		/* first look for determinate unfolding steps */
'selectionrule.homo-pure:select_positive_literal2'(Goal,_UnfHist,NrOfSel,SelLiteral) :-
	member_nr(SelLiteral,Goal,NrOfSel),
	not(is_negative_literal(SelLiteral,_Atom)),
	not(is_built_in_literal(SelLiteral)),
	undeterminate(Goal,NrOfSel).

'selectionrule.homo-pure:goal_can_be_unfolded'(_Goal,[]) :- !.
'selectionrule.homo-pure:goal_can_be_unfolded'(Goal,_UnfHist) :-
	partition_goal(Goal,SplitGoals),
	member(split_goal(SGoal,_Pos),SplitGoals),
	/* SGoal \== [Atom], */
	not(find_unimposed_instance(SGoal,_VariantID)),
	debug_print(not_variant(Goal)),debug_nl.

'selectionrule.homo-pure:ok_to_unfold'(SelLiteral,NrOfSel,UnfHist) :-
	not('selectionrule.homo-pure:embedded_covering_ancestor'(SelLiteral,NrOfSel,UnfHist)).

'selectionrule.homo-pure:embedded_covering_ancestor'(SelLiteral,NrOfSel,UnfHist) :-
	covering_ancestor(NrOfSel,UnfHist,CovAncestor),
	(homeomorphic_embedded(CovAncestor,SelLiteral)
	 -> (true)
	 ;  (/*SelLiteral = unfold(XU,_,_),not(XU=[]),
		print(not_embeddeding),nl,
		print(SelLiteral),nl,print(CovAncestor),
		nl,nl,*/fail )
	),
	assertz(unfolding_leads_to_loop(CovAncestor)).
	/* print(homo_embed(CovAncestor,SelLiteral)),nl. */
