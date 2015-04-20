:- module('check_instance_of.karp',['check_instance_of.karp:get_instance_of'/4]).

% :- dynamic get_instance_of/4.

:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../global_tree').

:- use_package( .('../ecce_no_rt2') ).


/* must be variant of something with same chtree */


'check_instance_of.karp:get_instance_of'(GoalID,Goal,Chtree,MoreGeneralID) :-
	node_is_an_abstraction(GoalID),
	copy(Goal,CGoal),
	gt_node_goal(MoreGeneralID,CGoal), /* lookup a potential match */
	ancestor_node(GoalID,MoreGeneralID),
	GoalID \== MoreGeneralID,
	gt_node_pe_status(MoreGeneralID,PEStatus),
	PEStatus \== no,
	gt_node_goal(MoreGeneralID,MoreGeneralGoal),
	variant_of(Goal,MoreGeneralGoal),
	gt_node_chtree(MoreGeneralID,Chtree). /* same chtree */



ancestor_node(GoalID,ParID2) :-
	gt_node_descends_from(GoalID,ParID2,LeafLocalID),
	not(LeafLocalID = chpos(abstracted,_)).
ancestor_node(GoalID,ParID3) :-
	gt_node_descends_from(GoalID,ParID2,_LeafLocalID),
	ancestor_node(ParID2,ParID3).
