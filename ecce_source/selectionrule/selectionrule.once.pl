:- module('selectionrule.once',['selectionrule.once:select_positive_literal'/5]).

:- use_package( .('../ecce_no_rt2') ).

%:- dynamic select_positive_literal/5.

:- set_prolog_flag(single_var_warnings,off).

:- use_module('../bimtools').
:- use_module('../calc_chtree').


'selectionrule.once:select_positive_literal'(Goal,TopGoalVarlist,UnfHist,NrOfSel,SelLiteral) :-
	'selectionrule.once:goal_can_be_unfolded'(Goal,UnfHist),
	member_nr(SelLiteral,Goal,NrOfSel),
	\+(is_negative_literal(SelLiteral,Atom)),
	\+(is_built_in_literal(SelLiteral)),
	debug_print(ok(SelLiteral)),debug_nl.

'selectionrule.once:goal_can_be_unfolded'(Goal,[]).

