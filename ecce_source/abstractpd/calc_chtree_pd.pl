:- module(calc_chtree_pd,_).

:- use_package( .('../ecce_no_rt2') ).


/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 2001           */
/* --------------------------------------------- */

/* TO GET THE LATEST VERSION OF THE ECCE SYSTEM GO TO:
	http://www.ecs.soton.ac.uk/~mal */
/* BUG REPORTS, QUESTIONS AND COMMENTS CAN BE SENT TO:
	mal@ecs.soton.ac.uk */

/* file: abstractpd/calc_chtree.pl */

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../global_tree').
:- use_module('../depth_bound').
:- use_module('../constraints').
:- use_module('../self_check').

:- use_module(flow_analysis).


/* ===================================================== */

:- include( '../multi_meta' ).


/* ===================================================== */

apd(Goal,Constraint) :-
   varlist(Goal,V),
   pp_mnf(calc_chtree(Goal,Constraint,V,[],Ctree)),
   print_chtree(Ctree),nl,
   print('Leaves: '),nl,
   pp_cll(leaf(Ctree,Goal,Constraint,Leaf,LeafC,Chpos,BUPC)),
   print('<'),print(Leaf),print(' , '),print(LeafC),
   print('> '), print(Chpos),nl, print(BUPC),nl,
   fail.
apd(_Goal,_Constraint).

/* ------------------ */
/* calculate_chtree/4 */
/* ------------------ */

pre_condition(calculate_chtree(GoalID,Goal,Constraint,_Chtree)) :-
	term_is_of_type(GoalID,nodeid),
	term_is_of_type(Constraint,constraint),
	term_is_of_type(Goal,goal).
post_condition(calculate_chtree(_GoalID,_Goal,_C,Chtree)) :-
	term_is_of_type(Chtree,chtree).

calculate_chtree(GoalID,Goal,Constraint,PrunedChtree) :-
        print(calculate_chtree(GoalID,Goal,Constraint)),nl,
	copy((Goal,Constraint),(CGoal,CConstraint)),
	varlist(CGoal,TopGoalVarlist),
	pp_mnf(calc_chtree(CGoal,CConstraint,TopGoalVarlist,[],Chtree)),
	debug_print_chtree(Chtree),
	copy(Goal,CGoal2),
	assertz(gt_node_user_info(GoalID,unpruned_chtree(Chtree))),
	pp_mnf(post_prune_chtree(CGoal2,Chtree,PrunedChtree)).


one_step_unfolding(_GoalID,Goal,Constraint,Chtree) :-
	current_depth_bound(CurDB),
	set_new_depth_bound(1),
	copy((Goal,Constraint),(CGoal,CConstraint)),
	varlist(CGoal,TopGoalVarlist),
	pp_mnf(calc_chtree(CGoal,CConstraint,TopGoalVarlist,[],Chtree)),
	set_new_depth_bound(CurDB).

/* ------------- */
/* calc_chtree/5 */
/* ------------- */

pre_condition(calc_chtree(G,C,TopGoalVarlist,UnfHist,_Chtree)) :-
	term_is_of_type(G,goal),
	term_is_of_type(C,constraint),
	term_is_of_type(TopGoalVarlist,list(any)),
	term_is_of_type(UnfHist,unfold_history).
post_condition(calc_chtree(_G,_C,TopGoalVarlist,_UnfHist,Chtree)) :-
	term_is_of_type(TopGoalVarlist,list(any)),
	term_is_of_type(Chtree,chtree).
	
self_check(must_succeed((assert(claus(3001,p(g(_X),c),[])),
   assert(claus(3002,p([],b),[])),assert(claus(3003,p([_|T],Y),[p(T,Y)]))))).
self_check(must_succeed(
   (pp_cll(calc_chtree([],[],[],[],C)),C=success))).
self_check(must_succeed(
   (pp_cll(calc_chtree([p(X,Y)],[ecce_type(cst([]),X)],[X,Y],[],C)),
     C=select(1,[match(3002,success)])))).
self_check(must_succeed(
   (pp_cll(calc_chtree([p(X,c)],[ecce_type(cst([]),X)],[X],[],C)),
     C=empty))).
self_check(must_succeed(
   (pp_cll(calc_chtree([p(X,b)],[ecce_type(list(any),X)],[X],[],C)),
     C=select(1,[match(3002,success),match(3003,stop)])))).
self_check(must_succeed((retract(claus(3001,p(g(_X),c),[])),
   retract(claus(3002,p([],b),[])),retract(claus(3003,p([_|T],Y),[p(T,Y)]))))).

calc_chtree([],_C,_TopGoalVarlist,_UnfHist,success).
calc_chtree([H|T],Constraint,TopGoalVarlist,UnfHist,Chtree) :-
        debug_print(calling_calc_chtree([H|T],Constraint)),debug_nl,
    ((pp_cll(more_specific_transformation([H|T])),
      \+(dead([H|T])) )
      ->
  /* MISSING: Look for simplifiable Built-in's and simplifiable Negations */
	(select_callable_built_in([H|T],NrOfBI,SelBI)
	 -> ((debug_print(calling_built_in(SelBI)),debug_nl,
	      call_built_in(SelBI))
	     -> (copy(SelBI,CSelBI),
		 IntUnfHist = [sel(CSelBI,NrOfBI,0,bi)|UnfHist],
		 pp_mnf(split_list([H|T],NrOfBI,Left,Sel,Right)),
		 append(Left,Right,NewGoal),
		 get_predicate(SelBI,BI),
		 calc_chtree(NewGoal,TopGoalVarlist,IntUnfHist,SubChtree),
		 ((SubChtree = empty)
		  -> (Chtree=empty)
		  ;  (Chtree=built_in_eval(NrOfBI,BI,SubChtree))
		 )
		)
	     ;  (Chtree=empty) /* calling the built-in failed */
	    )
	 ;
        (select_removable_literal([H|T],NrOfNegLit,SelNegLit)
	 -> (debug_print(removing_literal(SelNegLit)),debug_nl,
	     copy(SelNegLit,CSelNegLit),
	     IntUnfHist = [sel(CSelNegLit,NrOfNegLit,0,neg)|UnfHist],
	     pp_mnf(split_list([H|T],NrOfNegLit,Left,Sel,Right)),
	     append(Left,Right,NewGoal),
	     (is_negative_literal(SelNegLit,NgAtm)
		-> get_predicate(NgAtm,NegPred)
		;  get_predicate(SelNegLit,NegPred)
	     ),
	     calc_chtree(NewGoal,TopGoalVarlist,IntUnfHist,SubChtree),
	     ((SubChtree = empty)
		-> (Chtree=empty)
		;  (Chtree=remove(NrOfNegLit,NegPred,SubChtree))
	     )
	    )
	 ;  ((pp_cll(depth_bound_ok(UnfHist)),
	      (critical_bd_findall_nesting
		-> (print(bd_findall_critical),nl,fail) ; true),
	      pp_cll(select_positive_literal([H|T],TopGoalVarlist,UnfHist,
					NrOfSelLiteral,SelLit)) )
	      -> (debug_print(select_positive_literal(SelLit)),debug_nl,
		  copy(SelLit,CSelLit),
	          IntUnfHist = [sel(CSelLit,NrOfSelLiteral,0,DI)|UnfHist],
		  (undeterminate([H|T],NrOfSelLiteral)
		   -> (DI = nondet, debug_print(nd),
			bd_findall(CP,pp_cll(calc_chpath([H|T],
			             Constraint,TopGoalVarlist,
				     NrOfSelLiteral,IntUnfHist,CP)), Chpaths),
	               ((Chpaths=[])
	                -> (Chtree=empty)
	                ;  (Chtree=select(NrOfSelLiteral,Chpaths))
	               )
		      )
		   ;  (DI = det, debug_print(det),
			pp_cll(calc_chpath([H|T],Constraint,TopGoalVarlist,
					NrOfSelLiteral,IntUnfHist,CP))
			   -> (Chtree=select(NrOfSelLiteral,[CP]))
			   ;  (Chtree=empty)
		      )
		  )
	         )
	     ;  (Chtree=stop) /* select_positive_literal stopped unfolding */
	   )
	)
	)
     ; (Chtree=empty) /* msv or dead found out that the goal will always fail */
    ).



/* ------------- */
/* calc_chpath/6 */
/* ------------- */

pre_condition(calc_chpath(G,C,TopGoalVarlist,Nr,UnfHist,_Chpath)) :-
	term_is_of_type(G,goal),
	term_is_of_type(C,constraint),
	term_is_of_type(TopGoalVarlist,list(any)),
	(G=[] -> print('### Warning goal for calc_chpath is []') ; true),
	term_is_of_type(Nr,integer),
	term_is_of_type(UnfHist,unfold_history).
post_condition(calc_chpath(_G,_C,TopGoalVarlist,_Nr,_UnfHist,Chpath)) :-
	term_is_of_type(TopGoalVarlist,list(any)),
	term_is_of_type(Chpath,chpath).

calc_chpath(Goal,Constraint,TopGoalVarlist,NrOfSelLiteral,UnfHist,Chpath) :-
        debug_print(calc_chpath(Goal,Constraint)),debug_nl,
	pp_mnf(split_list(Goal,NrOfSelLiteral,Left,SelCall,Right)),
	debug_print(calc_chpath(SelCall)),debug_nl,
	peel_off_calls(SelCall,Sel),
	debug_print(peeled(Sel)),debug_nl,
	(is_negative_literal(Sel,_NegatedAtom)
	-> (print('### Error negative literal selected in calc_chpath'),nl,
	    print('### '),print(Sel),nl,fail)
	;  ((pp_cll(is_built_in_literal(Sel)))
	    -> (print('### Error built-in selected in calc_chpath'),nl,
	        print('### '),print(Sel),nl,fail)
	    ; true
           )
	),
	claus(ClauseNr,Sel,Body),
	debug_print(match(ClauseNr)),
	pp_mnf(append(Body,Right,IntGoal)),
	pp_mnf(append(Left,IntGoal,NewGoal)),
	pp_mnf(update_unfold_history(UnfHist,NrOfSelLiteral,
				Body,NewUnfHist)),
	(is_inf(TopGoalVarlist)
	 -> (print(' <OccurCheck> '),fail)
	 ;  true
	),
	l_peel_off_calls(NewGoal,PNewGoal),
	(simplify_constraint(Constraint,SConstraint)
	 -> true ;
	    (print(unsatisfiable(Constraint)),nl,fail)
	 ),
	project_constraint(SConstraint,PNewGoal,PConstraint),
	pp_mnf(calc_chtree(PNewGoal,PConstraint,TopGoalVarlist,NewUnfHist,SubTree)),
	(SubTree\=empty),
	Chpath=match(ClauseNr,SubTree).

	
/* ====================================================== */
/*       SOME SIMPLE TOOLS FOR THE SELECTION RULES        */
/* ====================================================== */


/* ------------------------ */
/* dead_positive_literal /4 */
/* ------------------------ */

/* detect dead postitive literals and return the dead literal (+ pos) */
/*  (a positive literal is dead if it doesn't match any clause) */

pre_condition(dead_positive_literal(Goal,C,_Literal,_NrOfSelLiteral)) :-
	term_is_of_type(Goal,goal),
	term_is_of_type(C,constraint).
post_condition(dead_positive_literal(_Goal,_C,Literal,NrOfSelLiteral)) :-
	term_is_of_type(Literal,nonvar),
	term_is_of_type(NrOfSelLiteral,integer).

dead_positive_literal(Goal,Constraint,Literal,NrOfSelLiteral) :-
	member_nr(SelLiteral,Goal,NrOfSelLiteral),
	peel_off_calls(SelLiteral,Literal),
	\+(is_negative_literal(Literal,_Atom)),
	\+(pp_cll(is_built_in_literal(Literal))),
	\+((claus(_Nr,Literal,_Body),satisfiable(Constraint))),
	print(dead_lit(Literal,Constraint)),nl.
	
	
/* ------ */
/* dead/2 */
/* ------ */

dead(Goal,C) :-
	pp_cll(dead_positive_literal(Goal,C,_AnyAtom,_AnySelLiteral)).
dead(Goal,_) :-
	pp_cll(dead_built_in(Goal,_AnyAtom,_AnySelLiteral)).
dead(Goal,_) :-
	pp_cll(dead_negative_literal(Goal,_AnyAtom,_AnySelLiteral)).
	
/* ------ */
/* live/2 */
/* ------ */

live(Goal,C) :-
	more_specific_transformation(Goal),
	\+(dead(Goal,C)).
	
/* --------------- */
/* undeterminate/3 */
/* --------------- */

/* tests wheter a given goal is undeterminate for a given
	selected literal */

pre_condition(undeterminate(Goal,C,_NrOfSel)) :-
	term_is_of_type(Goal,goal),
	term_is_of_type(C,constraint).
post_condition(undeterminate(_Goal,_C,NrOfSel)) :-
	term_is_of_type(NrOfSel,integer).

undeterminate(Goal,C,NrOfSel) :-
	copy((Goal,C),(Goal1,C1)),
	split_list(Goal1,NrOfSel,Left,SelCall,Right),
	peel_off_calls(SelCall,SelLit),
	\+(is_negative_literal(SelLit,_NegAtom)),
	\+(pp_cll(is_built_in_literal(SelLit))),
	claus(Nr,SelLit,Body),
	append(Body,Right,IntGoal),
	append(Left,IntGoal,NewGoal1),
	live(NewGoal1,C1),
	copy((Goal,C),(Goal2,C2)),
	/* now test if there is another live resolvent */
	copy(Goal,Goal2),
	split_list(Goal2,NrOfSel,Left2,SelCall2,Right2),
	peel_off_calls(SelCall2,SelLit2),
	claus(Nr2,SelLit2,Body2),
	(Nr\=Nr2),
	append(Body2,Right2,IntGoal2),
	append(Left2,IntGoal2,NewGoal2),
	live(NewGoal2,C2).
	
	
	
/* ============================================================ */
/*                   GETTING THE LEAVES                         */
/* ============================================================ */

:- dynamic bup_msg_changed/1.

reset_bup_msg_changed :- 
	retractall(bup_msg_changed(_)).

get_leaf(Chtree,Goal,Constraint,Leaf,LeafC,Chposition) :-
	reset_gennum(1),
	pp_cll(leaf(Chtree,Goal,Constraint,Leaf,LeafC,Chposition,BUPC)),
	\+(Chposition=empty).
	
get_all_leaves(Chtree,Goal,Constraint,Solutions) :-
     reset_gennum(1),
     findall( sol(Goal,Leaf,LeafC,Chposition,BUPC),
        pp_cll(leaf(Chtree,Goal,Constraint,Leaf,LeafC,Chposition,BUPC)),
	Solutions).

add_all_leaves(GoalID,[],MSG,BUPC) :-
     debug_print(msg(MSG,BUPC)),debug_nl,
     retract(gt_node_bup_cas(GoalID,(Goal,Constraint),(OldMSG,OldSC))),
     debug_print(old_gt_node_bup_cas(GoalID,(Goal,Constraint),(OldMSG,OldSC))),
     debug_nl,
     copy((Goal,Constraint),(G2,C2)),
     must_succeed(Goal=MSG),
     pp_mnf(constraint_union(Constraint,BUPC,UC)),
     (simplify_constraint(UC,SC)
      ->  (assertz(gt_node_bup_cas(GoalID,(G2,C2),(MSG,SC))),
           (variant_of((MSG,SC),(OldMSG,OldSC)) -> true
	    ;
	    (print(' *change* '),
	     assert(bup_msg_changed(GoalID))
	    ))
          )
      ;   assertz(gt_node_bup_cas(GoalID,(G2,C2),(G2,fail)))
     ),debug_print(done),debug_nl.
add_all_leaves(GoalID,[sol(Goal,Leaf,LeafC,ChPosition,BUPC)|T],MSGSoFar,BUPCSoFar) :-
  debug_print(sol(Goal,Leaf,LeafC,ChPosition,BUPC)),debug_nl,
	((Leaf = [])
	 -> (debug_print('### add_leaves: Leaf = []'),debug_nl)
	 ;  (gt_node_descends_from(ExistingChildID,GoalID,ChPosition)
	       -> ((gt_node_constraint(ExistingChildID,(G2,C2)),
	            variant_of((G2,C2),(Leaf,LeafC)))
		     ->
		      (print(' *exists* '),debug_nl) /* child already exists */
		     ; (
		      retract(gt_node_descends_from(ExistingChildID,GoalID,ChPosition)),
		      pp_mnf(add_gt_cleaf(GoalID,Leaf,LeafC,ChPosition,_LeafID)),
		      print(' *override* '),
		      print(added_new_leaf(_LeafID,of(GoalID))),nl
		      )
		     )
	       ;  (pp_mnf(add_gt_cleaf(GoalID,Leaf,LeafC,ChPosition,_LeafID)),
	           print(added_new_leaf(_LeafID,of(GoalID))),nl)
	       
	    )
	),
	pp_mnf(constraint_msg(MSGSoFar,BUPCSoFar,Goal,BUPC,NewMSG,NewBUPC)),
	add_all_leaves(GoalID,T,NewMSG,NewBUPC).

add_leaves(GoalID,Goal,Constraint,Chtree) :-
	get_all_leaves(Chtree,Goal,Constraint,Solutions),
	debug_print(got_all_leaves(Solutions)),debug_nl,
	(Solutions = [sol(G,_,_,_,_)|_] ->
	must_succeed(add_all_leaves(GoalID,Solutions,G,[]))
	; (debug_print(none),debug_nl)
	).
	
rl(X) :- recompute_leaves(X).	

recompute_leaves(GoalID) :-
     gt_node_pe_status(GoalID,VPEStat),
     (VPEStat \= no),
     (VPEStat \= abstracted(_)),
     \+(gt_node_instance_of(GoalID,_)),
     gt_node_constraint(GoalID,(Goal,Constraint)),
     gt_node_chtree(GoalID,Chtree),
     add_leaves(GoalID,Goal,Constraint,Chtree),
     fail.
recompute_leaves(GoalID) :- /* treat abstracted goals */
     node_is_abstracted(GoalID),
     print(GoalID),nl,
     bup_abstracted_node(GoalID,BUPGoal,BUPConstraint),
     retract(gt_node_bup_cas(GoalID,(Goal,Constraint),(OldMSG,OldSC))),
     debug_print(old_gt_node_bup_cas(GoalID,(Goal,Constraint),(OldMSG,OldSC))),
     debug_nl,
     (simplify_constraint(BUPConstraint,SC)
      ->  (assertz(gt_node_bup_cas(GoalID,(Goal,Constraint),(BUPGoal,SC))),
           (variant_of((BUPGoal,SC),(OldMSG,OldSC)) -> true
	    ;
	    (print(' *change* '),
	     assert(bup_msg_changed(GoalID))
	    ))
          )
      ;   assertz(gt_node_bup_cas(GoalID,(Goal,Constraint),(Goal,fail)))
      ),fail.
recompute_leaves(GoalID) :- /* treat instance_of goals */
     gt_node_instance_of(GoalID,InstanceOfId),
     print(inst(GoalID,InstanceOfId)),nl,
     retract(gt_node_bup_cas(GoalID,(Goal,Constraint),(OldMSG,OldSC))),
     print(old_gt_node_bup_cas(GoalID,(Goal,Constraint),(OldMSG,OldSC))),
     nl,
     copy(Goal,BUPGoal),
     gt_node_bup_cas(InstanceOfId,(_,_),(BUPGoal,BUPConstraint)),
     (simplify_constraint(BUPConstraint,SC)
      ->  (assertz(gt_node_bup_cas(GoalID,(Goal,Constraint),(BUPGoal,SC))),
           (variant_of((BUPGoal,SC),(OldMSG,OldSC)) -> true
	    ;
	    (print(' *change* '),
	     assert(bup_msg_changed(GoalID))
	    ))
          )
      ;   assertz(gt_node_bup_cas(GoalID,(Goal,Constraint),(Goal,fail)))
      ),fail.
recompute_leaves(_).    



bup_abstracted_node(GoalID,BUPGoal,BUPConstraint) :-
     gt_node_constraint(GoalID,(Goal,Constraint)),
     findall( (AID,Split),
              gt_node_descends_from(AID,GoalID,chpos(abstracted,Split)), As),
     print(bup_abstracted_node(GoalID,As)),nl,
     copy(Goal,BUPGoal),
     bup_abstracted_node2(As,BUPGoal,BUPConstraint).

bup_abstracted_node2([],BUPGoal,[]).
bup_abstracted_node2([(AID,Split)|T],BUPGoal,BUPConstraint) :-
   bup_abstracted_node2(T,BUPGoal,TC),
   split_goal(BUPGoal,Split,SplitGoal),
   print(split_goal(BUPGoal,Split,SplitGoal)),nl,
   gt_node_bup_cas(AID,(SplitGoal,_),(SplitGoal,SplitBUPC)),
   constraint_union(TC,SplitBUPC,BUPConstraint).
   
split_goal(G,SplitIndication,SplittedG) :-
   split_goal(G,1,SplitIndication,SplittedG).
split_goal([],_,_,[]).
split_goal([H|T],N,SplitInd,Res) :-
   (member(N,SplitInd) -> (Res = [H|ST]) ;
    (Res = ST)),
   N1 is N + 1,
   split_goal(T,N1,SplitInd,ST).
	
/* ------ */
/* leaf/6 */
/* ------ */

pre_condition(leaf(Chtree,Goal,C,_Leaf,_LC,_Chposition,_BUPC)) :-
	term_is_of_type(Chtree,chtree),
	term_is_of_type(C,constraint),
	term_is_of_type(Goal,goal).
post_condition(leaf(_Chtree,_Goal,_C,Leaf,LeafC,Chposition,BUPC)) :-
	term_is_of_type(Leaf,goal),
	term_is_of_type(LeafC,constraint),
	term_is_of_type(BUPC,constraint),
	(Chposition=empty ;
	 term_is_of_type(Chposition,chposition)).


leaf(empty,_Goal,_C,_Leaf,_LeafC,_ChPos,_BUPC) :- fail.
leaf(success,_Goal,_C,[],[],empty,[]). /* no leaves */
leaf(stop,Goal,Constraint,Leaf,LeafC,chpos(ResultantNr,SplitIndication),BUPC) :-
	gennum(ResultantNr),
	pp_cll(more_specific_transformation(Goal)),
	get_constraint_leaf(Goal,Constraint,Leaf,LeafC,SplitIndication,BUPC).
leaf(select(SelLitNr,Chpaths),Goal,Constraint,Leaf,LeafC,ChPos,BUPC) :-
	pp_mnf(more_specific_transformation(Goal)),
	pp_mnf(split_list(Goal,SelLitNr,Left,SelCall,Right)),
	pp_mnf(simplify_constraint(Constraint,SC)),
	peel_off_calls(SelCall,Sel),
	member(match(ClauseNr,SubTree),Chpaths),
	(claus(ClauseNr,Sel,Body) -> true
		; (print('### Error: clause not matching in leaf/4'),nl,
		   print('###  ClauseNr:'),print(ClauseNr),nl,
		   print('###  SelAtom: '),print(Sel),nl,fail)
	),
	pp_mnf(append(Body,Right,IntGoal)),
	pp_mnf(append(Left,IntGoal,NewGoal)),
	pp_mnf(project_constraint(SC,NewGoal,SPC)),
	pp_cll(leaf(SubTree,NewGoal,SPC,Leaf,LeafC,ChPos,BUPC)).
leaf(remove(SelLitNr,_Predicate,SubTree),Goal,Constraint,Leaf,LeafC,ChPos,BUPC) :-
	pp_cll(more_specific_transformation(Goal)),
	pp_mnf(split_list(Goal,SelLitNr,Left,_Sel,Right)),
/*	(is_negative_literal(Sel,NegAtom),
	 get_predicate(NegAtom,Predicate) -> (true)
		; (print('### Error: illegal negative literal in leaf/4'),nl,
		   print('###  '),print(Sel),print(' is not: '),
		   print_predicate(Predicate),nl)
	), */
	pp_mnf(append(Left,Right,NewGoal)),
	pp_cll(leaf(SubTree,NewGoal,Constraint,Leaf,LeafC,ChPos,BUPC)).
leaf(built_in_eval(SelLitNr,BI,SubTree),Goal,Constraint,Leaf,LeafC,ChPos,BUPC) :-
	pp_cll(more_specific_transformation(Goal)),
	pp_mnf(split_list(Goal,SelLitNr,Left,Sel,Right)),
	(get_predicate(Sel,BI) -> true
	 ;  (print('### Warning: illegal built-in in leaf/4'),nl,
	     print('###  '),print(Sel), print(' is not '),
	     print(BI),nl
	    )
	),
	(is_callable_built_in_literal(Sel)
		-> call_built_in(Sel)
		; (built_in_generates_bindings(Sel)
		   -> (print('### Error: illegal built-in in leaf/4'),nl,
		       print('###  '),print(Sel),
		       print(': generates bindings and is not callable'),nl
		      )
		    ;  true
		  )
	),
	pp_mnf(append(Left,Right,NewGoal)),
	pp_cll(leaf(SubTree,NewGoal,Constraint,Leaf,LeafC,ChPos,BUPC)).

	

partition_constraint_goal(Goal,_Constraint,SplittedGoals) :-
               pp_mnf(partition_goal(Goal,SplittedGoals)).

get_constraint_leaf(Goal,Constraint,Leaf,LeafC,SplitIndication,BUPC) :-
	pp_mnf(simplify_constraint(Constraint,SC)),
        partition_constraint_goal(Goal,Constraint,SplittedGoals),
	pp_cll(get_bup_computed_answers(SplittedGoals,SC,CSplittedGoals)),
	(member(split_goal(Leaf,LeafC,SplitIndication,fail),CSplittedGoals)
	 -> (BUPC = fail) /* return just one leaf */
	 ;
	 member(split_goal(Leaf,LeafC,SplitIndication,BUPC),CSplittedGoals)
	).
	
/* -------------------------- */
/* get_bup_computed_answers/3 */
/* -------------------------- */

ecce_type(csplit_goal,term(split_goal,[goal,constraint,split_indication,constraint])).

pre_condition(get_bup_computed_answers(SG,C,_BSG)) :-
	term_is_of_type(SG,list(split_goal)),
	term_is_of_type(C,constraint).
post_condition(get_bup_computed_answers(_SG,_C,BSG)) :-
	term_is_of_type(BSG,list(csplit_goal)).
	
	
/* DOESN'T YET HANDLE SPLIT GOALS WITHIN NEGATIONS !!! */
get_bup_computed_answers([],_Constraint,[]).
get_bup_computed_answers([split_goal(Leaf,SplitIndication)|T],Constraint,
                         [split_goal(Leaf,ILeafC,SplitIndication,BUPC)|BT]) :-
		pp_mnf(project_simplified_constraint(Constraint,Leaf,LeafC,Rem)),
		((find_unimposed_cinstance(Leaf,LeafC,VariantID) /*,
		  code_has_to_be_generated_for_node(VariantID) */)
		 ->
		  (debug_print('checking answers for '), debug_print(Leaf),
		   debug_print(' instance of '), debug_print(VariantID),debug_nl,
		   gt_node_bup_cas(VariantID,(Leaf,_),(Leaf,ILeafBUPC)),
		   debug_print(simplifying(ILeafBUPC)),debug_nl,
		   simplify_constraint(ILeafBUPC,ILeafC),debug_print(ok),debug_nl,
		   pp_mnf(append(ILeafC,Rem,UpdatedConstraint)),
		   BUPC = Constraint,
		   get_bup_computed_answers(T,UpdatedConstraint,BT)
		  )
		 ;
		 (print(no(Leaf)),ILeafC = LeafC, BUPC = fail, BT =[]
		   /* assume failure for BUP computation but still add leaf */
		   /* purely built-in's or external preds are not in split_goal */)
		 ).
		 
/* would be better to compute bup answers first and then partition the goal */
/* Also: currently no right-to-left propagation */
