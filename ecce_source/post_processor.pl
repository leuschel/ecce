:- module(post_processor,[
%pp_node_equivalent/2,pp_goals_splitted/0,
post_processing/0,print_transitions/0,perform_post_processing_minimising/0,reset_pp_goals_splitted/0,reset_node_equivalent/0,abstract_equivalent_goals/0,get_equivalence_class/1,smallest_element_of_equivalence_class/1,class_msg/3,add_abstraction_for_equivalence_class/2,make_goals_with_same_chtree_equivalent/0,split_discernable_goals/0,discernable_goal/2,transition/3,get_real_goal_id/2
]).

:- use_package( .(ecce_no_rt) ).


/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,1996,1997 */
/* --------------------------------------------- */

/* TO GET THE LATEST VERSION OF THE ECCE SYSTEM GO TO:
	http://www.cs.kuleuven.ac.be/~michael */
/* BUG REPORTS, QUESTIONS AND COMMENTS CAN BE SENT TO:
	michael@cs.kuleuven.ac.be */

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(bimtools).
:- use_module(global_tree).
:- use_module(calc_chtree).
:- use_module(chtree_tools).
:- use_module(code_generator).
:- use_module(parametric_files).

/* file: post_processor.pro */


/* minimises the polyvariance in the global tree */

:- include( multi_meta ).


:- dynamic pp_node_equivalent/2.
:- dynamic pp_goals_splitted/0.



post_processing :-
	(current_reduce_polyvariance(121)  /* 121 = y (Yes) */
	 -> (reset_node_equivalent,
	     make_goals_with_same_chtree_equivalent,
	     (debug_printing(on) -> print_transitions ; true),
	     perform_post_processing_minimising,
	     abstract_equivalent_goals,
	     verbose_nl
	   )
	 ;  true
	).


print_transitions :-
	transition(ParID,ChildID,ChPos),
	print(ParID),print(' --> '),print(ChildID),
	print(' : '),print(ChPos),nl,
	fail.
print_transitions.


/* ------------------------------------ */
/* perform_post_processing_minimising/0 */
/* ------------------------------------ */

perform_post_processing_minimising :-
	reset_pp_goals_splitted,
	split_discernable_goals,
	(pp_goals_splitted -> perform_post_processing_minimising ; true).


/* ------------------------- */
/* reset_pp_goals_splitted/0 */
/* ------------------------- */

pp_goals_splitted.

reset_pp_goals_splitted :-
	retract(pp_goals_splitted),fail.
reset_pp_goals_splitted.


/* ----------------------- */
/* reset_node_equivalent/0 */
/* ----------------------- */

reset_node_equivalent :-
	retract(pp_node_equivalent(_X,_Y)),fail.
reset_node_equivalent.


/* --------------------------- */
/* abstract_equivalent_goals/0 */
/* --------------------------- */

abstract_equivalent_goals :-
	get_equivalence_class(Class),
	debug_print(class(Class)),print('*'),debug_nl,
	Class = [GoalID1|Rest],
	gt_node_goal(GoalID1,Goal),
	class_msg(Rest,Goal,MSG),
	debug_println(msg(MSG)),
	mnf_call(change_gt_node_goal(GoalID1,MSG)),
	add_abstraction_for_equivalence_class(Class,GoalID1),
	fail.
abstract_equivalent_goals.


get_equivalence_class(Class) :-
	gt_node_goal(GoalID,_Goal),
	smallest_element_of_equivalence_class(GoalID),
	gt_node_chtree(GoalID,Chtree),
	Chtree \= empty,
	Class = [GoalID|RestClass],
	findall(EqID, pp_node_equivalent(GoalID,EqID), RestClass).


smallest_element_of_equivalence_class(GoalID) :-
	pp_node_equivalent(GoalID,_GoalID2),
	\+(pp_node_equivalent(_OtherGoalID,GoalID)),!.
	

class_msg([],MSG,MSG).
class_msg([GoalID1|T],MSGSoFar,MSG) :-
	gt_node_goal(GoalID1,Goal),
	msg(Goal,MSGSoFar,IMSG),
	class_msg(T,IMSG,MSG).


add_abstraction_for_equivalence_class([],_MoreGeneralID).
add_abstraction_for_equivalence_class([GoalID|T],MoreGeneralID) :-
	(GoalID=MoreGeneralID
	 -> true
	 ;  (mark_gt_node_as_instance_of(GoalID,MoreGeneralID),
		debug_print(inst_of(GoalID,MoreGeneralID)),print('+'),debug_nl)
	),
	add_abstraction_for_equivalence_class(T,MoreGeneralID).


/* ---------------------------------------- */
/* make_goals_with_same_chtree_equivalent/0 */
/* ---------------------------------------- */

make_goals_with_same_chtree_equivalent :-
	gt_node_chtree(GoalID,Chtree),
	\+(has_builtins_which_generate_bindings(Chtree)),
	code_has_to_be_generated_for_node(GoalID),
	gt_node_chtree(GoalID2,Chtree),
		/* Missing: improve to detect similar chtrees */
	GoalID @< GoalID2,
	gt_node_goal(GoalID,Goal1),
	gt_node_goal(GoalID2,Goal2),
	msg_can_be_taken(Goal1,Goal2),
	code_has_to_be_generated_for_node(GoalID2),
	assert(pp_node_equivalent(GoalID,GoalID2)),
	debug_println(equiv(GoalID,GoalID2)),
	fail.
make_goals_with_same_chtree_equivalent.


/* ------------------------- */
/* split_discernable_goals/0 */
/* ------------------------- */

split_discernable_goals :-
	discernable_goal(GoalID1,GoalID2),
	retract(pp_node_equivalent(GoalID1,GoalID2)),
	debug_print(split(GoalID1,GoalID2)),
	verbose_print('.'),debug_nl,
	(pp_goals_splitted -> true ; assert(pp_goals_splitted)),
	fail.
split_discernable_goals.

/* ------------------ */
/* discernable_goal/2 */
/* ------------------ */

/* test whether two goals set as equivalent can be discerned by some
   transition */
discernable_goal(GoalID1,GoalID2) :-
	pp_node_equivalent(GoalID1,GoalID2),
	transition(GoalID1,ChildID1,ChPos),
	(transition(GoalID2,ChildID2,ChPos)
	 -> \+(equivalent_pp_nodes(ChildID1,ChildID2))
	 ;  true
	).
discernable_goal(GoalID1,GoalID2) :-
	pp_node_equivalent(GoalID1,GoalID2),
	transition(GoalID2,_ChildID2,ChPos),
	\+(transition(GoalID1,_ChildID1,ChPos)).

equivalent_pp_nodes(GoalID,GoalID) :- !.
equivalent_pp_nodes(GoalID1,GoalID2) :-
	GoalID1 @< GoalID2,
	pp_node_equivalent(GoalID1,GoalID2),!.
equivalent_pp_nodes(GoalID1,GoalID2) :-
	GoalID2 @< GoalID1,
	pp_node_equivalent(GoalID2,GoalID1),!.

/* ------------ */
/* transition/3 */
/* ------------ */

transition(ParID,ChildID,ChPos) :-
	gt_node_descends_from(GoalID,ParID,ChPos),
	code_has_to_be_generated_for_node(ParID),
	pp_mnf(get_real_goal_id(GoalID,ChildID)).  


/* ------------------ */
/* get_real_goal_id/2 */
/* ------------------ */

pre_condition(get_real_goal_id(GoalID,_RealID)) :-
	term_is_of_type(GoalID,nodeid).
post_condition(get_real_goal_id(_GoalID,RealID)) :-
	term_is_of_type(RealID,nodeid).

get_real_goal_id(GoalID,GoalID) :-
	code_has_to_be_generated_for_node(GoalID),!.
get_real_goal_id(GoalID,RealID) :-
	gt_node_instance_of(GoalID,GenID),!,
	get_real_goal_id(GenID,RealID).
get_real_goal_id(GoalID,RealID) :-
	gt_node_pe_status(GoalID,abstracted(_Imp)),
	gt_node_descends_from(AbstractedID,GoalID,SplitInd),
		/* does not allow splitting abstractions yet !!! */
	get_real_goal_id(AbstractedID,RealID).
