:- module( 'selectionrule.det-bot-top' , [ 'selectionrule.det-bot-top:select_positive_literal'/5 ] ).
/* file: selectionrule.det-bot-top.pro */

:- dynamic select_positive_literal/5.

:- use_package( .('../ecce_no_rt2') ).

%not(Goal) :- \+(Goal).


/* A simple determinate unfolding rule w/o a depth bound */

'selectionrule.det-bot-top:select_positive_literal'(Goal,TopGoalVarlist,UnfHist,NrOfSel,SelLiteral) :-
	/* Try to find determinate literals */
	member_nr(SelLiteral,Goal,NrOfSel),
	not(is_negative_literal(SelLiteral,Atom)),
	not(is_built_in_literal(SelLiteral)),
	'selectionrule.det-bot-top:ok_to_unfold'(Goal,NrOfSel,UnfHist),
	not('selectionrule.det-bot-top:embedded_covering_ancestor'(SelLiteral,NrOfSel,UnfHist)),
	debug_print(unfold(SelLiteral)),debug_nl.
'selectionrule.det-bot-top:select_positive_literal'(Goal,TopGoalVarlist,UnfHist,NrOfSel,SelLiteral) :-
	/* Try to find determinate literals */
	member_nr(SelLiteral,Goal,NrOfSel),
	not(is_negative_literal(SelLiteral,Atom)),
	not(is_built_in_literal(SelLiteral)),
	not(find_unimposed_variant([SelLiteral],VariantID)),
	ok_to_unfold2(Goal,NrOfSel,UnfHist),
	not('selectionrule.det-bot-top:embedded_covering_ancestor'(SelLiteral,NrOfSel,UnfHist)),
	debug_print(unfold2(SelLiteral)),debug_nl.

'selectionrule.det-bot-top:ok_to_unfold'(Goal,NrOfSel,[]) :- !.
'selectionrule.det-bot-top:ok_to_unfold'(Goal,NrOfSel,UnfHist) :-
	not(undeterminate(Goal,NrOfSel)),
	not(find_unimposed_variant([SelLiteral],VariantID)),
	( not(pp_cll(contains_non_determinate_step(UnfHist)))
	   /* ;
	 not(pp_cll(goal_increasing_selection(Goal,NrOfSel)))  */
	).

ok_to_unfold2(Goal,1,UnfHist) :-
	not(pp_cll(contains_non_determinate_step(UnfHist))),
	length(Goal,L),
	L>1.
ok_to_unfold2(Goal,NrOfSel,UnfHist) :-
	not(undeterminate(Goal,NrOfSel)),
	pp_cll(contains_non_determinate_step_at_top(UnfHist)),
	length(Goal,L),
	L>1.

'selectionrule.det-bot-top:embedded_covering_ancestor'(SelLiteral,NrOfSel,UnfHist) :-
	covering_ancestor(NrOfSel,UnfHist,CovAncestor),
	homeomorphic_embedded(CovAncestor,SelLiteral),
	debug_print(embed(CovAncestor,SelLiteral)),debug_nl.
