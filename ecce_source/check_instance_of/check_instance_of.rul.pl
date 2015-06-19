:- module('check_instance_of.rul',['check_instance_of.rul:get_instance_of'/4]).

:- include('header.pl').

%:- use_module('../rul/INTERFACE').

%%%:- ecce_use_module('rul/ecceRUL').
%%%:- ecce_use_module('rul/instance_entails').

%:- initialization(retractall(rul_active(_)),assert(rul_active(yes))).

%:- dynamic get_instance_of/4.

:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../global_tree').


/* must be instance of something with same chtree */

'check_instance_of.rul:get_instance_of'(GoalID,Goal,Chtree,MoreGeneralID) :-
	Chtree \== stop, /* this will happen for goals like [X] */
        divide_constraint_rul_goal(Goal,OrdGoal,ConstrGoal),
	copy(OrdGoal,COrdGoal),
	numbervars(COrdGoal,1,_),
	gt_node_goal(MoreGeneralID,MGoal),
	GoalID \== MoreGeneralID,
	gt_node_pe_status(MoreGeneralID,PEStatus),
	PEStatus \== no, \+(PEStatus = abstracted(_)),
	divide_constraint_rul_goal(MGoal,COrdGoal,MCGoal),
	 /* above checks whether ordinary part is an instance */
	/* could be pre-processed */
	'check_instance_of.rul:instance_is_ok'(MoreGeneralID,PEStatus,Chtree),
	/* now check constraint entailment */
	debug_println(check_instance_of(GoalID,MoreGeneralID)),
	gt_node_goal(MoreGeneralID,MoreGeneralGoal),
	divide_constraint_rul_goal(MoreGeneralGoal,MOG,MCG),
	debug_print('instance_of_entails'(OrdGoal,ConstrGoal,MOG,MCG)),
	debug_nl,
	/*entails(OrdGoal,ConstrGoal,MOG,MCG),*/
	instance_of_entails(OrdGoal,ConstrGoal,MOG,MCG),
	debug_println('instance_of_entails succeeds'). 

'check_instance_of.rul:instance_is_ok'(_MoreGeneralID,PEStat,_Chtree) :-
	PEStat \== pe(imposed), PEStat \== abstracted(imposed).
		/* otherwise chtree might be incorrect */
'check_instance_of.rul:instance_is_ok'(MoreGeneralID,_PEStat,Chtree) :-
	gt_node_chtree(MoreGeneralID,Chtree).




/* --------- For Abstract Partial Deduction: -------------- */


get_cinstance_of(GoalID,Goal,Constraint,Chtree,MoreGeneralID) :-
	Chtree \== stop, /* this will happen for goals like [X] */
	copy(Goal,CGoal),
	numbervars(CGoal,1,_),
	gt_node_goal(MoreGeneralID,CGoal),
	GoalID \== MoreGeneralID,
	gt_node_pe_status(MoreGeneralID,PEStatus),
	PEStatus \== no,
	'check_instance_of.rul:instance_is_ok'(MoreGeneralID,PEStatus,Chtree),
	gt_node_constraint(MoreGeneralID,(MG,MGC)),
        constraint_instance_of(Goal,Constraint,MG,MGC). 
