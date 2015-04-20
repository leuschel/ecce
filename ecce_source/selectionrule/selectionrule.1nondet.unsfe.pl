:- module('selectionrule.1nondet.unsfe',['selectionrule.1nondet.unsfe:select_positive_literal'/5]).

:- use_package( .('../ecce_no_rt2') ).


%:- dynamic select_positive_literal/5.

:- set_prolog_flag(single_var_warnings,off).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../unfold_history').


/* A simple determinate unfolding rule w/o a depth bound w/o homeo. embedding */

'selectionrule.1nondet.unsfe:select_positive_literal'(Goal,TopGoalVarlist,UnfHist,NrOfSel,SelLiteral) :-
	/* Try to find determinate literals */
	member_nr(SelLiteral,Goal,NrOfSel),
	not(is_negative_literal(SelLiteral,Atom)),
	not(is_built_in_literal(SelLiteral)),
	'selectionrule.1nondet.unsfe:ok_to_unfold'(Goal,NrOfSel,UnfHist).

'selectionrule.1nondet.unsfe:ok_to_unfold'(Goal,NrOfSel,[]) :- !.
'selectionrule.1nondet.unsfe:ok_to_unfold'(Goal,NrOfSel,UnfHist) :-
	not(pp_cll(contains_non_determinate_step(UnfHist))),
	not(pp_cll(goal_increasing_selection(Goal,NrOfSel))).
		/* then one will certainly profit from the extra information
			obtained by unfolding */
	/* print(ok_non_det(UnfHist)),nl. */
'selectionrule.1nondet.unsfe:ok_to_unfold'(Goal,NrOfSel,UnfHist) :-
	not(undeterminate(Goal,NrOfSel)),
	( not(pp_cll(contains_non_determinate_step(UnfHist)))
	 ;
	 not(pp_cll(goal_increasing_selection(Goal,NrOfSel)))
	).


