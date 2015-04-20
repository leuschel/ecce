:- module('check_instance_of.instchtr',['check_instance_of.instchtr:get_instance_of'/4]).

% :- dynamic get_instance_of/4.

:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../global_tree').

/*
:- multifile gt_node/1,gt_node_goal/2,gt_node_constraint/2,
             gt_node_bup_cas/3,gt_node_descends_from/3,
             gt_node_instance_of/2,gt_node_chtree/2,gt_node_pe_status/2,
	     gt_node_user_info/2.             

:- dynamic gt_node/1.
:- dynamic gt_node_goal/2.
:- dynamic gt_node_constraint/2.
:- dynamic gt_node_bup_cas/3.
:- dynamic gt_node_descends_from/3.
:- dynamic gt_node_instance_of/2.
:- dynamic gt_node_chtree/2.
:- dynamic gt_node_pe_status/2.
:- dynamic gt_node_user_info/2.
*/

/* must be instance of something with same chtree */

'check_instance_of.instchtr:get_instance_of'(GoalID,Goal,Chtree,MoreGeneralID) :-
	Chtree \== stop, /* this will happen for goals like [X] */
	copy(Goal,CGoal),
	numbervars(CGoal,1,_),
	gt_node_goal(MoreGeneralID,CGoal),
	GoalID \== MoreGeneralID,
	gt_node_pe_status(MoreGeneralID,PEStatus),
	PEStatus \== no,
	'check_instance_of.instchtr:instance_is_ok'(MoreGeneralID,PEStatus,Chtree). 

'check_instance_of.instchtr:instance_is_ok'(MoreGeneralID,PEStat,Chtree) :-
	gt_node_chtree(MoreGeneralID,Chtree).
