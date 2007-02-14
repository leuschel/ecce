:- module('abstract.naivemsg',['abstract.naivemsg:abstract_parent'/6,'abstract.naivemsg:abstract_leaf'/6]).

/* for reproducing Petri net algorithms by Finkel, Karp-Miller */

%:- dynamic abstract_parent/6.
%:- dynamic abstract_leaf/6.

:- set_prolog_flag(multi_arity_warnings,off).
%:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../global_tree').

:- use_module( 'abstract.common.pl' ).

'abstract.naivemsg:abstract_parent'(GoalID,Goal,Chtree,WhistleGoalID,
		NewWhistleGoals,NewWhistleChtrees) :- 
	gt_node_goal(WhistleGoalID,WhistleGoal),
	goal_naivemsg(Goal,WhistleGoal,MSG),
	(variant_of(MSG,WhistleGoal)
	 -> (debug_print('abstract_parent unsuccessful'),debug_nl,fail)
	 ;  (debug_print(abstract_parent(wg(WhistleGoal),nmsg(MSG))),debug_nl)
	),
	NewWhistleGoals = [split_goal(MSG,FSI)],
	get_full_split_indicator(MSG,1,FSI),
	NewWhistleChtrees = [none].

'abstract.naivemsg:abstract_leaf'(GoalID,Goal,Chtree,WhistleGoalID,NewGoals,NewChtrees) :-
	gt_node_goal(WhistleGoalID,WhistleGoal),
	goal_naivemsg(Goal,WhistleGoal,MSG),
	NewGoals = [split_goal(MSG,FSI)],
	get_full_split_indicator(MSG,1,FSI),
	NewChtrees = [none].

goal_naivemsg([],[],[]) :- !.
goal_naivemsg([X|XA],[Y|YA],[Z|ZA]) :- !,
	naivemsg(X,Y,Z), debug_print(naivemsg(X,Y,Z)),debug_nl,
	goal_naivemsg(XA,YA,ZA).
goal_naivemsg(X,Y,Z) :- naivemsg(X,Y,Z).

naivemsg(X,Y,Z) :-
	X=..[Func|XA],
	Y=..[Func|YA],
	l_naivemsg(XA,YA,ZA),!,
	Z=..[Func|ZA].
naivemsg(X,Y,_Z) :- print('*** error naivemsg failed'),nl.

l_naivemsg([],[],[]).
l_naivemsg([X|XA],[Y|YA],[X|ZA]) :-
	variant_of(X,Y),!,l_naivemsg(XA,YA,ZA).
l_naivemsg([X|XA],[Y|YA],[_|ZA]) :-
	l_naivemsg(XA,YA,ZA).
