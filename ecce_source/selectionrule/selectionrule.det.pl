:- module('selectionrule.det',[ 'selectionrule.det:select_positive_literal'/5]).

:- use_package( .('../ecce_no_rt2') ).


%:- dynamic select_positive_literal/5.

:- set_prolog_flag(single_var_warnings,off).

:- use_module('../bimtools').
:- use_module('../calc_chtree').


/* A simple determinate unfolding rule w/o a depth bound */

'selectionrule.det:select_positive_literal'(Goal,_TopGoalVarlist,UnfHist,NrOfSel,SelLiteral) :-
	/* Try to find determinate literals */
	member_nr(SelLiteral,Goal,NrOfSel),
	not(is_negative_literal(SelLiteral,Atom)),
	not(is_built_in_literal(SelLiteral)),
	'selectionrule.det:ok_to_unfold'(Goal,NrOfSel,UnfHist).

'selectionrule.det:ok_to_unfold'(Goal,NrOfSel,[]).  /* allow non-determinate unfolding at top */
'selectionrule.det:ok_to_unfold'(Goal,NrOfSel,_UnfHist) :-
	not(undeterminate(Goal,NrOfSel)).
