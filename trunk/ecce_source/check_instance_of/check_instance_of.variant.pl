:- module('check_instance_of.variant',['check_instance_of.variant:get_instance_of'/4]).

%:- dynamic get_instance_of/4.

:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../global_tree').


/* must be variant of something with same chtree */

'check_instance_of.variant:get_instance_of'(GoalID,Goal,Chtree,MoreGeneralID) :-
	copy(Goal,CGoal),
	gt_node_goal(MoreGeneralID,CGoal), /* lookup a potential match */
	GoalID \== MoreGeneralID,
	gt_node_pe_status(MoreGeneralID,PEStatus),
	PEStatus \== no,
	gt_node_goal(MoreGeneralID,MoreGeneralGoal),
	variant_of(Goal,MoreGeneralGoal),
	gt_node_chtree(MoreGeneralID,Chtree). /* same chtree */
