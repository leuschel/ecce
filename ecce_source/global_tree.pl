:- module(global_tree,
	[
	    gt_node/1,
	    gt_node_goal/2,
	    gt_node_constraint/2,
	    gt_node_bup_cas/3,
	    gt_node_descends_from/3,
	    gt_node_instance_of/2,
	    gt_node_chtree/2,
	    gt_node_pe_status/2,
	    gt_node_user_info/2,  
	    init_gt/0,
	    delete_gt_node/1,
	    remove_gt_leaves/1,
	    get_gt_goal_to_pe/2,
	    get_gt_cgoal_to_pe/3,
	    get_nr_of_gt_nodes_to_pe/1,
	    add_gt_root/2,
	    add_gt_croot/3,
	    add_gt_leaf/4,
	    add_gt_cleaf/5,
	    node_is_an_abstraction/1,
	    node_is_abstracted/1,
	    mark_gt_node_as_ped/3,
	    mark_gt_node_as_instance_of/2,	    
	    new_gt_node/2,
	    new_gt_cnode/3,
	    change_gt_node_goal/2,
	    print_gt_nodes/0,
	    print_gt_node/1,
	    print_gt_node_summary/1,
	    print_gt_node_bup/1,
	    print_gt_nodes_as_graph_for_dot/0,
	    gt_generate_dot_file/1
	]).

:- use_package( .(ecce_no_rt) ).


/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,1996,1997 */
/* --------------------------------------------- */

/* TO GET THE LATEST VERSION OF THE ECCE SYSTEM GO TO:
	http://www.cs.kuleuven.ac.be/~michael */
/* BUG REPORTS, QUESTIONS AND COMMENTS CAN BE SENT TO:
	michael@cs.kuleuven.ac.be */

/* file: global_tree.pro */

/* takes care of the global tree structure for partial deduction */

/*
:- ensure_consulted('$BIMTOOLS_PATH/gensym.pro').
:- ensure_consulted('$BIMTOOLS_PATH/StdLists.pro').
:- ensure_consulted('$BIMTOOLS_PATH/typechecker.pro').
:- ensure_consulted('$BIMTOOLS_PATH/prepost.pro').
*/
:- use_module(bimtools).

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).


:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(dec10_io)).

:- use_module(dynpreds).
:- use_module(code_generator).
:- use_module(dot_generator).

:- use_module(main_functions).
%:- use_module(constraints).
:- use_module(calc_chtree).
:- use_module(chtree_tools).

:- use_module(self_check).

:- include( multi_meta ).


/*
:- multifile gt_node/1,gt_node_goal/2,gt_node_constraint/2,
             gt_node_bup_cas/3,gt_node_descends_from/3,
             gt_node_instance_of/2,gt_node_chtree/2,gt_node_pe_status/2,
	       gt_node_user_info/2.
*/

:- dynamic gt_node/1.
:- dynamic gt_node_goal/2.
:- dynamic gt_node_constraint/2.
:- dynamic gt_node_bup_cas/3.
:- dynamic gt_node_descends_from/3.
:- dynamic gt_node_instance_of/2.
:- dynamic gt_node_chtree/2.
:- dynamic gt_node_pe_status/2.
:- dynamic gt_node_user_info/2.

ecce_type(goal,list(literal)).
ecce_type(literal,nonvar).
ecce_type(nodeid,ground).
ecce_type(leafid,ground).

pre_condition(gt_node_pe_status(_GoalID,_PEStatus)).
post_condition(gt_node_pe_status(GoalID,PEStatus)) :-
	term_is_of_type(GoalID,nodeid),
	term_is_of_type(PEStatus,pe_status).

self_check(must_succeed(init_gt)).
self_check(must_succeed( (
    pp_mnf(add_gt_croot([p(Z)],[ecce_type(list(any),Z)],NodeID)),
    pp_mnf(mark_gt_node_as_ped(NodeID,pe(unimposed),stop)),
    pp_mnf(add_gt_cleaf(NodeID,[q(Z),r(Z)],[ecce_type(list(any),Z)],chpos(1,[1,2]),_LeafID))))).
self_check(must_succeed(print_gt_nodes)).
self_check(must_succeed( (
    get_gt_cgoal_to_pe(_GID,G,C), C=[ecce_type(list(any),Z)],
    G = [q(Z),r(Z)]))).
self_check(must_succeed(init_gt)).
self_check(must_succeed( (
    pp_mnf(add_gt_root([p(Z)],NodeID)),
    pp_mnf(mark_gt_node_as_ped(NodeID,pe(unimposed),stop)),
    pp_mnf(add_gt_leaf(NodeID,[q(Z),r(Z)],chpos(1,[1,2]),_LeafID))))).
self_check(must_succeed( (
    get_gt_cgoal_to_pe(_GID,G,C), C=[],
    G = [q(Z),r(Z)]))).
self_check(init_gt).

/* --------- */
/* init_gt/0 */
/* --------- */

init_gt :-
	retract(gt_node(_ID)),fail.
init_gt :-
	retract(gt_node_goal(_ID,_Goal)),fail.
init_gt :-
	retract(gt_node_constraint(_ID,_C)),fail.
init_gt :-
	retract(gt_node_bup_cas(_ID,_CG,_BCG)),fail.
init_gt :-
	retract(gt_node_descends_from(_ID,_ParID,_LeafLocalID)),fail.
init_gt :-
	retract(gt_node_instance_of(_ID,_GenID)),fail.
init_gt :-
	retract(gt_node_chtree(_ID,_Chtree)),fail.
init_gt :-
	retract(gt_node_pe_status(_ID,_Status)),fail.
init_gt :-
	retract(gt_node_user_info(_ID,_UI)),fail.
init_gt.

/* ---------------- */
/* delete_gt_node/1 */
/* ---------------- */

delete_gt_node(ID) :-
	retract(gt_node(ID)),fail.
delete_gt_node(ID) :-
	retract(gt_node_goal(ID,_Goal)),fail.
delete_gt_node(ID) :-
	retract(gt_node_constraint(ID,_C)),fail.
delete_gt_node(ID) :-
	retract(gt_node_bup_cas(ID,_CG,_BCG)),fail.
delete_gt_node(ID) :-
	retract(gt_node_descends_from(ID,_ParID,_LeafLocalID)),fail.
delete_gt_node(ID) :-
	retract(gt_node_descends_from(_DescID,ID,_LeafLocalID)),fail.
delete_gt_node(ID) :-
	retract(gt_node_instance_of(ID,_GenID)),fail.
delete_gt_node(ID) :-
	retract(gt_node_instance_of(_InstID,ID)),fail.
delete_gt_node(ID) :-
	retract(gt_node_chtree(ID,_Chtree)),fail.
delete_gt_node(ID) :-
	retract(gt_node_pe_status(ID,_Chtree)),fail.
delete_gt_node(ID) :-
	retract(gt_node_user_info(ID,_UI)),fail.
delete_gt_node(_ID).

/* ------------------ */
/* remove_gt_leaves/1 */
/* ------------------ */

pre_condition(remove_gt_leaves(GoalID)) :-
	term_is_of_type(GoalID,nodeid).
post_condition(remove_gt_leaves(_GoalID)).

remove_gt_leaves(ID) :-
	debug_print(remove_leaves(ID)),
	retract(gt_node_descends_from(DescID,ID,_LeafLocalID)),
	remove_gt_leaves(DescID),
	delete_gt_node(DescID),
	fail.
remove_gt_leaves(_ID).

/* ------------------- */
/* get_gt_goal_to_pe/2 */
/* ------------------- */

pre_condition(get_gt_goal_to_pe(_GoalID,_Goal)).
post_condition(get_gt_goal_to_pe(GoalID,Goal)) :-
	term_is_of_type(GoalID,nodeid),
	term_is_of_type(Goal,goal).

get_gt_goal_to_pe(GoalID,Goal) :-
	gt_node_pe_status(GoalID,no),
	gt_node_goal(GoalID,Goal).

/* ------------------- */
/* get_gt_cgoal_to_pe/3 */
/* ------------------- */

pre_condition(get_gt_cgoal_to_pe(_GoalID,_Goal,_C)).
post_condition(get_gt_cgoal_to_pe(GoalID,Goal,C)) :-
	term_is_of_type(GoalID,nodeid),
	term_is_of_type(Goal,goal),
	term_is_of_type(C,constraint).

get_gt_cgoal_to_pe(GoalID,Goal,C) :-
	gt_node_pe_status(GoalID,no),
	gt_node_goal(GoalID,Goal),
	gt_node_constraint(GoalID,(Goal,C)).

/* -------------------------- */
/* get_nr_of_gt_nodes_to_pe/1 */
/* -------------------------- */

get_nr_of_gt_nodes_to_pe(X) :-
	findall(1,gt_node_pe_status(_GoalID,no),Ls),
	length(Ls,X).

get_node_level(NodeID,Level) :-
	(gt_node_descends_from(NodeID,ParID,_)
	-> (get_node_level(ParID,PL),Level is PL + 1)
	;  (Level = 0)
	).

/* ------------- */
/* add_gt_root/2 */
/* ------------- */

pre_condition(add_gt_root(Goal,GoalNodeID)) :-
	term_is_of_type(Goal,goal),
	term_is_of_type(GoalNodeID,var).
post_condition(add_gt_root(_PGoal,GoalNodeID)) :-
	term_is_of_type(GoalNodeID,nodeid).

add_gt_root(Goal,GoalNodeID) :-
	pp_mnf(add_gt_leaf(root,Goal,local_root,GoalNodeID)).

/* -------------- */
/* add_gt_croot/3 */
/* -------------- */

pre_condition(add_gt_croot(Goal,Constraint,GoalNodeID)) :-
	term_is_of_type(Goal,goal),
	term_is_of_type(Constraint,constraint),
	term_is_of_type(GoalNodeID,var).
post_condition(add_gt_croot(_PGoal,_Constraint,GoalNodeID)) :-
	term_is_of_type(GoalNodeID,nodeid).

add_gt_croot(Goal,Constraint,GoalNodeID) :-
	pp_mnf(add_gt_cleaf(root,Goal,Constraint,local_root,GoalNodeID)).

/* ------------- */
/* add_gt_leaf/4 */
/* ------------- */

pre_condition(add_gt_leaf(ParentNodeID,LeafGoal,LeafLocalID,LeafID)) :-
	term_is_of_type(ParentNodeID,nodeid),
	term_is_of_type(LeafGoal,goal),
	term_is_of_type(LeafLocalID,chposition),
	term_is_of_type(LeafID,var).
post_condition(add_gt_leaf(_ParentNodeID,_LeafGoal,_LeafLocalID,LeafID)) :-
	term_is_of_type(LeafID,nodeid).

add_gt_leaf(ParentNodeID,LeafGoal,LeafLocalID,LeafID) :-
	pp_mnf(new_gt_node(LeafGoal,LeafID)),
	assertz(gt_node_descends_from(LeafID,ParentNodeID,LeafLocalID)).

/* -------------- */
/* add_gt_cleaf/5 */
/* -------------- */

pre_condition(add_gt_cleaf(ParentNodeID,LeafGoal,LeafC,LeafLocalID,LeafID)) :-
	term_is_of_type(ParentNodeID,nodeid),
	term_is_of_type(LeafGoal,goal),
	term_is_of_type(LeafC,constraint),
	term_is_of_type(LeafLocalID,chposition),
	term_is_of_type(LeafID,var).
post_condition(add_gt_cleaf(_ParentNodeID,_LeafGoal,_C,_LeafLocalID,LeafID)) :-
	term_is_of_type(LeafID,nodeid).

add_gt_cleaf(ParentNodeID,LeafGoal,LeafC,LeafLocalID,LeafID) :-
	pp_mnf(new_gt_cnode(LeafGoal,LeafC,LeafID)),
	assertz(gt_node_descends_from(LeafID,ParentNodeID,LeafLocalID)).

/* ------------------------ */
/* node_is_an_abstraction/1 */
/* ------------------------ */

node_is_an_abstraction(GoalID) :-
	gt_node_descends_from(GoalID,_ParentNodeID,chpos(abstracted,_)).
/* true if GoalID resulted from abstracting and not unfolding */

node_is_abstracted(GoalID) :-	
        gt_node_pe_status(GoalID,abstracted(_)).

/* --------------------- */
/* mark_gt_node_as_ped/3 */
/* --------------------- */
ecce_type(pe_status,term(abstracted,[impose_status])).
ecce_type(pe_status,term(no,[])).
ecce_type(pe_status,term(pe,[impose_status])).

ecce_type(impose_status,term(imposed,[])).
ecce_type(impose_status,term(unimposed,[])).

ecce_type(ext_chtree,term(none,[])).
ecce_type(ext_chtree,chtree).


pre_condition(mark_gt_node_as_ped(NodeID,Status,Chtree)) :-
	term_is_of_type(NodeID,nodeid),
	term_is_of_type(Status,pe_status),
	term_is_of_type(Chtree,ext_chtree),
	((Status\=no, Chtree=none)
	 -> (print('### Warning: marking partially evaluated goal'),nl,
	     print('###          with chtree = none'),nl,fail
	    )
	 ;  true
	).
post_condition(mark_gt_node_as_ped(_NodeID,_Status,_Chtree)).

mark_gt_node_as_ped(NodeID,Status,_Chtree) :-
	retract(gt_node_pe_status(NodeID,Stat)),
	(((Stat = no) ; (Status = abstracted(_))) -> true
	 ;(print('### WARNING: Node already partially evaluated for mark_gt_node_as_ped'),nl,
	   print_gt_node(NodeID))
	),fail.
mark_gt_node_as_ped(NodeID,_Status,_Chtree) :-
	retract(gt_node_chtree(NodeID,_EarlierChtree)),fail.
mark_gt_node_as_ped(NodeID,Status,Chtree) :-
	assertz(gt_node_chtree(NodeID,Chtree)),
	asserta(gt_node_pe_status(NodeID,Status)).

/* ----------------------------- */
/* mark_gt_node_as_instance_of/2 */
/* ----------------------------- */

pre_condition(mark_gt_node_as_instance_of(NodeID,InstanceOfID)) :-
	term_is_of_type(NodeID,nodeid),
	term_is_of_type(InstanceOfID,nodeid),
	((NodeID = InstanceOfID)
	 -> (print('### Warning self-loop added in mark_gt_node_as_instance_of'),nl,
	     print('### NodeID = '), print(NodeID),nl,fail
	    )
	 ;  true
	),
	((gt_node_pe_status(InstanceOfID,abstracted(_ImpStat)),
	  gt_node_descends_from(NodeID,InstanceOfID,_SplitInd))
	 -> (print('### Warning loop added in mark_gt_node_as_instance_of'),nl,
	     print('### NodeID = '), print(NodeID),nl,
	     print('### InstanceOfID = '), print(InstanceOfID),nl,fail
	    )
	 ;  true
	).
post_condition(mark_gt_node_as_instance_of(_NodeID,_InstanceOfID)).

mark_gt_node_as_instance_of(NodeID,InstanceOfID) :-
	assertz(gt_node_instance_of(NodeID,InstanceOfID)).


/* -------------------------- INTERNAL TOOLS ------------------------------ */


/* ------------- */
/* new_gt_node/2 */
/* ------------- */

pre_condition(new_gt_node(Goal,NodeID)) :-
	term_is_of_type(Goal,goal),
	term_is_of_type(NodeID,var).
post_condition(new_gt_node(_Goal,NodeID)) :-
	term_is_of_type(NodeID,nodeid).

new_gt_node(Goal,NodeID) :-
	pp_mnf(new_gt_cnode(Goal,[],NodeID)).

/* -------------- */
/* new_gt_cnode/3 */
/* -------------- */

pre_condition(new_gt_cnode(Goal,Constraint,NodeID)) :-
	term_is_of_type(Goal,goal),
	term_is_of_type(Constraint,constraint),
	term_is_of_type(NodeID,var).
post_condition(new_gt_cnode(_Goal,_Constraint,NodeID)) :-
	term_is_of_type(NodeID,nodeid).

new_gt_cnode(Goal,Constraint,NodeID) :-
	gensym(gt_id,NodeID),
	assertz(gt_node(NodeID)),
	assertz(gt_node_goal(NodeID,Goal)),
	assertz(gt_node_constraint(NodeID,(Goal,Constraint))),
	assertz(gt_node_bup_cas(NodeID,(Goal,Constraint),(Goal,fail))),
	assertz(gt_node_chtree(NodeID,none)),
	asserta(gt_node_pe_status(NodeID,no)).


/* change the Goal of a node: used e.g. by postprocessing polyvariance reduction */
change_gt_node_goal(NodeID,NewGoal) :-
	retract(gt_node_goal(NodeID,_OldGoal)),
	assert(gt_node_goal(NodeID,NewGoal)),
	retract(gt_node_bup_cas(NodeID,_,_)),
	assertz(gt_node_bup_cas(NodeID,(NewGoal,[]),(NewGoal,fail))),
	retract(gt_node_constraint(NodeID,_)),
	assertz(gt_node_constraint(NodeID,(NewGoal,[]))).
	
/* ---------------- */
/* print_gt_nodes/0 */
/* ---------------- */

print_gt_nodes :- 
	gt_node(NodeID),
	print_gt_node(NodeID),
	fail.
print_gt_nodes :- pgn. /* output .dot file */

/* --------------- */
/* print_gt_node/1 */
/* --------------- */
%:- dynamic print_gt_node/1.


print_gt_node(NodeID) :-
	nl,print('NodeID = '), print(NodeID), print(' '),
	gt_node_goal(NodeID,Goal), nl,
	numbervars(Goal,0,_),
	print('Goal = '), print(Goal), nl,
	(gt_node_constraint(NodeID,(Goal,C)) -> true
          ; ((C=[]) -> true ; (print('Constraint = '),print(C),nl))),
	(gt_node_bup_cas(NodeID,(Goal,C),(IGoal,CAS))
         -> ((CAS=fail) -> (print(fail),nl) ; (print('BUP CAS = <'),
             print(IGoal),print(' , '), print(CAS), print(' >'),nl))
          ;  (print(none),nl)),
	print('Descends from: '),fail.
print_gt_node(NodeID) :-
	gt_node_descends_from(NodeID,DescID,_LeafLocalID),
	print_gt_node_summary(DescID),fail.
print_gt_node(_NodeID) :-
	nl,print('Instance of: '),fail.
print_gt_node(NodeID) :-
	gt_node_instance_of(NodeID,GenID),
	print_gt_node_summary(GenID),fail.
print_gt_node(NodeID) :-
	nl,gt_node_pe_status(NodeID,Status),
	print('PE Status = '), print(Status),nl.

	
/* ----------------------- */
/* print_gt_node_summary/1 */
/* ----------------------- */
print_gt_node_summary(root) :-
	print('<root>'),!.
print_gt_node_summary(NodeID) :-
	print('<'), print(NodeID), print(','),
	gt_node_goal(NodeID,Goal),
	print(Goal), print('>').


/* ------------------- */
/* print_gt_node_bup/1 */
/* ------------------- */
print_gt_node_bup :- print('BUP Summary:'),nl,print_gt_node_bup(_X),fail.
print_gt_node_bup.

print_gt_node_bup(NodeID) :-
	gt_node_goal(NodeID,_),
	gt_node_bup_cas(NodeID,(Goal,C),(IGoal,CAS)),
	print('<'), print(NodeID), print(': ('),
	print(Goal),
	((C=[]) -> true ; (print(','),print(C))),
	print(') ---> ('),
        ((CAS = fail) -> print(fail)
          ;(print(IGoal),print(','),print(CAS))
	), print(') >'),
	(code_has_to_be_generated_for_node(NodeID) -> print(' code') ; true),
	(node_is_abstracted(NodeID) -> print(' abs') ; true),
	nl.





/* --------------------------------- */
/* print_gt_nodes_as_graph_for_dot/0 */
/* --------------------------------- */


pgn :- %tell('~/demo/ecce.gtree.dot'),
	(print_gt_nodes_as_graph_for_dot -> true ; true),
	 true.
   %told.
   % system('/Applications/Development/Graphviz/dot -Tps ~/demo/ecce.gtree.dot -o ~/demo/ecce.gtree.ps').

gt_generate_dot_file(F) :-
   tell(F),
   (print_gt_nodes_as_graph_for_dot -> true ; true),
   told.
   

print_gt_nodes_as_graph_for_dot :-
    print('digraph global_tree {'),nl,
   % print('graph [orientation=landscape, page="8.5, 11",ratio=fill,size="7.5,10"];'),nl,
    print('graph [page="8.5, 11",ratio=fill,size="7.5,10"];'),nl,
	gt_node(NodeID),
	print_gt_node_as_graph_for_dot(NodeID),
	fail.
print_gt_nodes_as_graph_for_dot :-
    print('}'),nl.
    

is_a_leaf(NodeID) :- \+(gt_node_descends_from(_,NodeID,_)).

print_gt_node_as_graph_for_dot(NodeID) :-
	gt_node_goal(NodeID,_Goal), 
	(is_a_leaf(NodeID) -> generate_dot_nodes_for_leaves(yes) ; true),
	print_gt_node_as_graph_for_dot2(NodeID).
	
print_gt_node_as_graph_for_dot2(NodeID) :-
	gt_node_goal(NodeID,Goal), 
	numbervars(Goal,0,_),
	gt_node_pe_status(NodeID,Status),
	((Status = pe(_))
	 -> (code_has_to_be_generated_for_node(NodeID)
	     -> (Shape = box,
	           (get_filtered_version(Goal,NodeID,FGoal)
	             -> (Color = blue)
	             ;  (/* code removed by dead code elimination or similar */
	                 FGoal = none, Color = green
	                )
	            )
	         )
	     ;  (Shape = triangle, Color = green, FGoal = none)
	     )
	 ;  (Shape = ellipse, Color = red, FGoal = none)
	 ),
    print(NodeID), print(' [shape='),print(Shape),
    print(', color='),print(Color),
    print(', label="'), 
    (gt_print_node_ids(yes)
     -> (print(NodeID), print(':'), print('\\'),print('n'))
     ;  true
    ),
    ((FGoal=none) -> true ; (print(FGoal),print(' = '), print('\\'),print('n'))),
    print_goal_for_dot(Goal),
    print('"];'),nl,
    fail.
print_gt_node_as_graph_for_dot2(NodeID) :-
	gt_node_descends_from(NodeID,DescID,_LeafLocalID),
	DescID \= root,
	print(DescID), print(' -> '),
	print(NodeID), print(';'),nl,fail.
print_gt_node_as_graph_for_dot2(NodeID) :-
	gt_node_descends_from(ChildID,NodeID,chpos(_,_)),
	get_filtered_goal_id(ChildID,FilteredID),
	print(NodeID), print(' -> '),
	print(FilteredID), print(' [style = dotted, color = blue];'),
	nl,fail.
print_gt_node_as_graph_for_dot2(NodeID) :-
	gt_node_instance_of(NodeID,GenID),
	print(GenID), print(' -> '),
	print(NodeID), print(' [style = dotted];'),
	nl,fail.
print_gt_node_as_graph_for_dot2(_NodeID) :- nl.


print_goal_for_dot([]).
print_goal_for_dot([A]) :- !,print_atom_for_dot(A).
print_goal_for_dot([H|T]) :- print_atom_for_dot(H),
	print(',\\'),print('n'),
	print_goal_for_dot(T).

print_atom_for_dot(rul__constraint__declaration(C,_R)) :- !,
	print(rul(C)).
print_atom_for_dot(A) :- print(A).
