:- module('check_instance_of.clpfd',['check_instance_of.clpfd:get_instance_of'/4]).

%:- use_module(library(clpfd)).

%:- retractall(clpfd_active(_)),assert(clpfd_active(yes)).

% :- dynamic get_instance_of/4.

:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../global_tree').
:- use_module('../constraints').
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

'check_instance_of.clpfd:get_instance_of'(GoalID,Goal,Chtree,MoreGeneralID) :-
	Chtree \== stop, /* this will happen for goals like [X] */
        divide_constraint_goal(Goal,OrdGoal,ConstrGoal),
	copy(OrdGoal,COrdGoal),
	numbervars(COrdGoal,1,_),
	gt_node_goal(MoreGeneralID,MGoal),
	GoalID \== MoreGeneralID,
	print(asserted(H)),nl,
	gt_node_pe_status(MoreGeneralID,PEStatus),
	PEStatus \== no, \+(PEStatus = abstracted(_)),
	divide_constraint_goal(MGoal,COrdGoal,MCGoal),
	 /* above checks whether ordinary part is an instance */
	/* could be pre-processed */
	'check_instance_of.clpfd:instance_is_ok'(MoreGeneralID,PEStatus,Chtree),
	/* now check constraint entailment */
	print(check_instance_of(GoalID,MoreGeneralID)),nl,
	gt_node_goal(MoreGeneralID,MoreGeneralGoal),
	divide_constraint_goal(MoreGeneralGoal,MOG,MCG),
	print(check_entails(OrdGoal,ConstrGoal,MOG,MCG)),nl,
	entails(OrdGoal,ConstrGoal,MOG,MCG),print(yes),nl. 

'check_instance_of.clpfd:instance_is_ok'(_MoreGeneralID,PEStat,_Chtree) :-
	PEStat \== pe(imposed), PEStat \== abstracted(imposed).
		/* otherwise chtree might be incorrect */
'check_instance_of.clpfd:instance_is_ok'(MoreGeneralID,_PEStat,Chtree) :-
	gt_node_chtree(MoreGeneralID,Chtree).




/* --------- For Abstract Partial Deduction: -------------- */

/*
'check_instance_of.clpfd:get_cinstance_of'(GoalID,Goal,Constraint,Chtree,MoreGeneralID) :-
	Chtree \== stop, /* this will happen for goals like [X] */
	copy(Goal,CGoal),
	numbervars(CGoal,1,_),
	gt_node_goal(MoreGeneralID,CGoal),
	GoalID \== MoreGeneralID,
	gt_node_pe_status(MoreGeneralID,PEStatus),
	PEStatus \== no,
	instance_is_ok(MoreGeneralID,PEStatus,Chtree),
	gt_node_constraint(MoreGeneralID,(MG,MGC)),
        constraint_instance_of(Goal,Constraint,MG,MGC). 
*/
