:- module(calc_chtree, [
        calculate_chtree/3,
        one_step_unfolding/3,
        calc_chtree/4,
        select_callable_built_in/3,
        select_removable_literal/3,
        select_removable_negative_literal/3,
        select_redundant_literal/4,
        calc_chpath/5,
        debug_print_chtree/1,
        print_chtree/1,
        print_chtree/2,
        print_chpaths/2,
        dead_positive_literal/3,
        dead_negative_literal/3,
        dead_built_in/3,
        dead/1,
        live/1,
        undeterminate/2,
        has_matching_rule/2,
        reduces_search_space/2,
        goal_increasing_selection/2,
        goal_contains_undefined_literal/1,
        is_undefined_literal/1,
        is_negative_literal/2,
        is_built_in_literal/1,
        is_constraint_literal/1,
        keep_constraint_literal_in_residual_program/1,
        divide_constraint_goal/3,
        divide_constraint_residual_goal/3,
        call_built_in/1,
        is_callable_built_in_literal/1,
        built_in_generates_bindings/1,
        simplify_built_in_literal/2,
        get_predicate/2,
        get_predicate_through_calls/2,
        atoms_have_same_predicate/3,
        msg_can_be_taken/2,
        print_predicate/1,
        sharing/2,
        connected_sub_goal/3,
        sub_goal/2,
        var_can_be_added_to_subgoal/2,
        get_leaf/4,
        leaf/4,
        extract_positive_atom_from_literal/2,
        peel_off_calls/2,
        l_peel_off_calls/2,
        partition_goal/2,
        get_literal_numbers/3
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


:- use_module(library(lists)).
:- use_module(library(dynamic)).

/* file: calc_chtree.pro */


/*
:- ensure_consulted('$BIMTOOLS_PATH/typechecker.pro').
:- ensure_consulted('$BIMTOOLS_PATH/prepost.pro').
:- ensure_consulted('$BIMTOOLS_PATH/bd_findall.pro').
:- ensure_consulted('$BIMTOOLS_PATH/StdLists.pro').
*/
:- use_module(bimtools).
:- use_module(dynpreds).

:- use_module(global_tree).
:- use_module(static_dynamic_functors).
:- use_module(depth_bound).
:- use_module(unfold_history).
:- use_module(code_generator).
:- use_module(main_functions).

:- use_module('abstract/abstract').
:- use_module('check_instance_of/check_instance_of').
:- use_module('more_specific/more_specific').
:- use_module('neg_solve/neg_solve').
:- use_module('partition/partition').
:- use_module('selectionrule/selectionrule').
:- use_module('postprune/post_prune').
:- use_module('whistle/whistle').


:- use_module(self_check).

/* ===================================================== */

:- include( multi_meta ).

/* ===================================================== */

%:- multifile more_specific_transformation/1.
%:- dynamic more_specific_transformation/1.



/* ==================== */
/* CHARACTERISTIC TREES */
/* ==================== */

/* ++++++++++++++++ */
/* TYPE DEFINITIONS */
/* ++++++++++++++++ */
ecce_type(selected_literal_nr,integer).
ecce_type(clause_nr,integer).
ecce_type(chtree,term(success,[])).
ecce_type(chtree,term(stop,[])).
ecce_type(chtree,term(empty,[])).
ecce_type(chtree,term(select,[selected_literal_nr,list(chpath)])).
ecce_type(chtree,term(remove,[selected_literal_nr,predicate,children])).
ecce_type(chtree,term(built_in_eval,[selected_literal_nr,predicate,children])).
/* ecce_type(chtree,term(built_in_simplify,[selected_literal_nr,predicate,children])). */
ecce_type(chpath,term(match,[clause_nr,children])).
ecce_type(children,chtree).
/* ++++++++++++++++ */



/* ------------------ */
/* calculate_chtree/3 */
/* ------------------ */

pre_condition(calculate_chtree(GoalID,Goal,_Chtree)) :-
	term_is_of_type(GoalID,nodeid),
	term_is_of_type(Goal,goal).
post_condition(calculate_chtree(_GoalID,_Goal,Chtree)) :-
	term_is_of_type(Chtree,chtree).

calculate_chtree(GoalID,Goal,PrunedChtree) :- 
	copy(Goal,CGoal),
	varlist(CGoal,TopGoalVarlist),
	pp_mnf(calc_chtree(CGoal,TopGoalVarlist,[],Chtree)),
	copy(Goal,CGoal2),
	assertz(gt_node_user_info(GoalID,unpruned_chtree(Chtree))),
	pp_mnf(post_prune_chtree(CGoal2,Chtree,PrunedChtree)).


one_step_unfolding(_GoalID,Goal,Chtree) :-
	current_depth_bound(CurDB),
	set_new_depth_bound(1),
	copy(Goal,CGoal),
	varlist(CGoal,TopGoalVarlist),
	pp_mnf(calc_chtree(CGoal,TopGoalVarlist,[],Chtree)),
	set_new_depth_bound(CurDB).


/* ------------- */
/* calc_chtree/3 */
/* ------------- */

pre_condition(calc_chtree(G,TopGoalVarlist,UnfHist,_Chtree)) :-
	term_is_of_type(G,goal),
	term_is_of_type(TopGoalVarlist,list(any)),
	term_is_of_type(UnfHist,unfold_history).
post_condition(calc_chtree(_G,TopGoalVarlist,_UnfHist,Chtree)) :-
	term_is_of_type(TopGoalVarlist,list(any)),
	term_is_of_type(Chtree,chtree).


calc_chtree([],_TopGoalVarlist,_UnfHist,success).
calc_chtree([H|T],TopGoalVarlist,UnfHist,Chtree) :- 
    ((pp_cll(more_specific_transformation([H|T])),
      (detect_dead_literals_or_non_leftmost_builtins(no) -> true ; \+(dead([H|T]))) )
      ->
  /* MISSING: Look for simplifiable Built-in's and simplifiable Negations */
	(select_callable_built_in([H|T],NrOfBI,SelBI)
	 -> ((debug_print(calling_built_in(SelBI)),debug_nl,
	      call_built_in(SelBI))
	     -> (copy(SelBI,CSelBI),
		 IntUnfHist = [sel(CSelBI,NrOfBI,0,bi)|UnfHist],
		 pp_mnf(split_list([H|T],NrOfBI,Left,Sel,Right)),
		 append(Left,Right,NewGoal),
		 get_predicate_through_calls(SelBI,BI),
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
		   -> (DI = nondet, debug_print(non_deterministic),debug_nl,
			bd_findall(CP,pp_cll(calc_chpath([H|T],TopGoalVarlist,
				  NrOfSelLiteral,IntUnfHist,CP)), Chpaths),
	               ((Chpaths=[])
	                -> Chtree=empty
	                ;  Chtree=select(NrOfSelLiteral,Chpaths)
	               )
		      )
		   ;  (DI = det, debug_print(deterministic),debug_nl,
			calc_chpath([H|T],TopGoalVarlist,
					NrOfSelLiteral,IntUnfHist,CP)
			   -> Chtree=select(NrOfSelLiteral,[CP])
			   ;  Chtree=empty
		      )
		  )
	         )
	     ;  (Chtree=stop) /* select_positive_literal stopped unfolding */
	   )
	)
	)
     ; (Chtree=empty) /* msv or dead found out that the goal will always fail */
    ).



select_callable_built_in([H|T],NrOfBI,SelectedBI) :-
	(is_callable_built_in_literal(H)
	 -> (NrOfBI = 1, SelectedBI = H)
	 ;  (detect_dead_literals_or_non_leftmost_builtins(yes),
	     select_callable_built_in2(T,TNr,SelectedBI),
	     NrOfBI is TNr + 1)
	).
select_callable_built_in2([H|T],NrOfBI,SelectedBI) :-
	(is_callable_built_in_literal(H)
	 -> (NrOfBI = 1, SelectedBI = H)
	 ;  (select_callable_built_in(T,TNr,SelectedBI),
	     NrOfBI is TNr + 1)
	).


select_removable_literal(Goal,NrOfNL,SelectedLit) :-
	select_removable_negative_literal(Goal,NrOfNL,SelectedLit),!.
select_removable_literal(Goal,NrOfNL,SelectedLit) :-
	select_redundant_literal(Goal,NrOfNL,SelectedLit,[]),!.

select_removable_negative_literal([H|T],NrOfNL,SelectedLit) :-
	((is_negative_literal(H,Atom),
	  pp_mnf(neg_solve([Atom],SolveSol)),
	  SolveSol = fail
	 )
	 -> (NrOfNL = 1, SelectedLit = H /* , print(remove(H)),nl */)
	 ;  (select_removable_negative_literal(T,TNr,SelectedLit),
	     NrOfNL is TNr + 1)
	).

select_redundant_literal([H|T],NrOfNL,SelectedLit,Prev) :-
	(redundant_call(H,Prev)
	-> (NrOfNL = 1, SelectedLit = H)
	;  (select_redundant_literal(T,TNr,SelectedLit,[H|Prev]),
	    NrOfNL is TNr + 1
	   )
	).


/* ------------- */
/* calc_chpath/5 */
/* ------------- */

pre_condition(calc_chpath(G,TopGoalVarlist,Nr,UnfHist,_Chpath)) :-
	term_is_of_type(G,goal),
	term_is_of_type(TopGoalVarlist,list(any)),
	(G=[] -> print('### Warning goal for calc_chpath is []') ; true),
	term_is_of_type(Nr,integer),
	term_is_of_type(UnfHist,unfold_history).
post_condition(calc_chpath(_G,TopGoalVarlist,_Nr,_UnfHist,Chpath)) :-
	term_is_of_type(TopGoalVarlist,list(any)),
	term_is_of_type(Chpath,chpath).

calc_chpath(Goal,TopGoalVarlist,NrOfSelLiteral,UnfHist,Chpath) :-
    debug_print(call_split_list(Goal,NrOfSelLiteral,Left,SelCall,Right)),debug_nl,
	pp_mnf(split_list(Goal,NrOfSelLiteral,Left,SelCall,Right)),
	debug_print(calc_chpath(SelCall)),debug_nl,
	peel_off_calls(SelCall,Sel),
	%debug_print(peeled(Sel)),debug_nl,
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
	pp_mnf(append(Body,Right,IntGoal)),
	pp_mnf(append(Left,IntGoal,NewGoal)),
	pp_mnf(update_unfold_history(UnfHist,NrOfSelLiteral,
				Body,NewUnfHist)),
	(is_inf(TopGoalVarlist)
	 -> (print(' <OccurCheck> '),fail)
	 ;  true
	),
	l_peel_off_calls(NewGoal,PNewGoal),
	pp_mnf(calc_chtree(PNewGoal,TopGoalVarlist,NewUnfHist,SubTree)),
	SubTree \= empty,
	Chpath=match(ClauseNr,SubTree).



debug_print_chtree(Chtree) :-
	(debug_printing(on)
	 -> (print_chtree(Chtree))
	 ;  (true)
	).

/* -------------- */
/* print_chtree/1 */
/* -------------- */

print_chtree(Chtree) :- print_chtree(Chtree,0).

print_chtree(Chtree,Level) :-
	((term_is_of_type(Chtree,chtree),term_is_of_type(Level,sint))
	 -> (true) ; (print('### Type error in print_chtree/1'),nl)
	),
	fail.
print_chtree(none,Level) :- /* extended chtrees only */
	indent(Level),
	print('**NONE**'),nl.
print_chtree(empty,Level) :-
	indent(Level),
	print('**empty**'),nl.
print_chtree(stop,Level) :-
	indent(Level),
	print('*stop*'),nl.
print_chtree(success,Level) :-
	indent(Level),
	print('*success*'),nl.
print_chtree(select(Nr,Chpaths),Level) :-
	indent(Level),
	print(''),print(Nr),print(' <- selected'),nl,
	print_chpaths(Chpaths,s(Level)).
print_chtree(remove(Nr,Pred,Chtree),Level) :-
	indent(Level),
	print(''),print(Nr),print('-'),
	print_predicate(Pred),
	print(' <- negative or redundant literal selected'),nl,
	print_chtree(Chtree,s(Level)).
print_chtree(built_in_eval(Nr,BuiltIn,Chtree),Level) :-
	indent(Level),
	print(''),print(Nr),print('-'),
	print_predicate(BuiltIn),print(' <- built-in evaluated'),nl,
	print_chtree(Chtree,s(Level)).
print_chtree(built_in_simplify(Nr,BuiltIn,Chtree),Level) :-
	indent(Level),
	print(''),print(Nr),print('-'),
	print_predicate(BuiltIn),print(' <- built-in simplified'),nl,
	print_chtree(Chtree,s(Level)).

print_chpaths(Chpaths,Level) :-
	term_is_of_type(Chpaths,list(chpath)),
	term_is_of_type(Level,sint),
	fail.
print_chpaths([],_Level).
print_chpaths([match(Nr,Chtree)|Rest],Level) :-
	indent(Level),
	print(match(Nr)),nl,
	print_chtree(Chtree,Level),
	print_chpaths(Rest,Level).


ecce_type(sint,term(0,[])).
ecce_type(sint,term(s,[sint])).

pre_condition(indent(X)) :-
	term_is_of_type(X,sint).
post_condition(indent(_X)).

/* indent(X) :- term_is_of_type(X,sint),fail. */

indent(0).
indent(s(X)) :- print('| '),indent(X).


/* ====================================================== */
/*       SOME SIMPLE TOOLS FOR THE SELECTION RULES        */
/* ====================================================== */


/* ------------------------ */
/* dead_positive_literal /3 */
/* ------------------------ */

/* detect dead postitive literals and return the dead literal (+ pos) */
/*  (a positive literal is dead if it doesn't match any clause) */

pre_condition(dead_positive_literal(Goal,_Literal,_NrOfSelLiteral)) :-
	term_is_of_type(Goal,goal).
post_condition(dead_positive_literal(_Goal,Literal,NrOfSelLiteral)) :-
	term_is_of_type(Literal,nonvar),
	term_is_of_type(NrOfSelLiteral,integer).

dead_positive_literal(Goal,Literal,NrOfSelLiteral) :-
	member_nr(SelLiteral,Goal,NrOfSelLiteral),
	peel_off_calls(SelLiteral,Literal),
	\+(is_negative_literal(Literal,_Atom)),
	\+(pp_cll(is_built_in_literal(Literal))),
	\+(claus(_Nr,Literal,_Body)).
	/* print(dead_lit(Literal)),nl. */

/* ------------------------ */
/* dead_negative_literal /3 */
/* ------------------------ */

/* detect dead negative literals and return the dead literal (+ pos) */

pre_condition(dead_negative_literal(Goal,_Literal,_NrOfSelLiteral)) :-
	term_is_of_type(Goal,goal).
post_condition(dead_negative_literal(_Goal,Literal,NrOfSelLiteral)) :-
	term_is_of_type(Literal,nonvar),
	term_is_of_type(NrOfSelLiteral,integer).

dead_negative_literal(Goal,Literal,NrOfSelLiteral) :-
	member_nr(Literal,Goal,NrOfSelLiteral),
	is_negative_literal(Literal,Atom),
	pp_mnf(neg_solve([Atom],SolveSol)),
	SolveSol = success.
	/* print(dead_nl(Literal)),nl. */


/* ---------------- */
/* dead_built_in /3 */
/* ---------------- */

/* detect dead built-in literals and return the dead literal (+ pos) */
/*  (a built-in is dead if it can be evaluated and fails) */

pre_condition(dead_built_in(Goal,_Literal,_NrOfSelLiteral)) :-
	term_is_of_type(Goal,goal).
post_condition(dead_built_in(_Goal,Literal,NrOfSelLiteral)) :-
	term_is_of_type(Literal,nonvar),
	term_is_of_type(NrOfSelLiteral,integer).

dead_built_in(Goal,Literal,NrOfSelLiteral) :-
	member_nr(Literal,Goal,NrOfSelLiteral),
	is_callable_built_in_literal(Literal),
	\+(call_built_in(Literal)).
	/* print(dead_bi(Literal)),nl. */

/* ------ */
/* dead/1 */
/* ------ */

dead(Goal) :-
   (detect_dead_literals_or_non_leftmost_builtins(no) -> SelLit=1 ; true),
   dead2(Goal,SelLit).

dead2(Goal,SelLiteral) :- 
	pp_cll(dead_positive_literal(Goal,_AnyAtom,SelLiteral)).
dead2(Goal,SelLiteral) :- 
	pp_cll(dead_built_in(Goal,_AnyAtom,SelLiteral)).
dead2(Goal,SelLiteral) :- 
	pp_cll(dead_negative_literal(Goal,_AnyAtom,SelLiteral)).

/* ------ */
/* live/1 */
/* ------ */

live(Goal) :-
	more_specific_transformation(Goal),
	\+(dead(Goal)).

/* --------------- */
/* undeterminate/2 */
/* --------------- */

/* tests wheter a given goal is undeterminate for a given
	selected literal */

pre_condition(undeterminate(Goal,_NrOfSel)) :-
	term_is_of_type(Goal,goal).
post_condition(undeterminate(_Goal,NrOfSel)) :-
	term_is_of_type(NrOfSel,integer).

undeterminate(Goal,NrOfSel) :- 
	copy(Goal,Goal1),
	split_list(Goal1,NrOfSel,Left,SelCall,Right),
	peel_off_calls(SelCall,SelLit),
	\+(is_negative_literal(SelLit,_NegAtom)),
	\+(pp_cll(is_built_in_literal(SelLit))),
	claus(Nr,SelLit,Body),
	append([Left,Body,Right],NewGoal),
	live(NewGoal),
	/* now test if there is another live resolvent */
	copy(Goal,Goal2),
	split_list(Goal2,NrOfSel,Left2,SelCall2,Right2),
	peel_off_calls(SelCall2,SelLit2),
	claus(Nr2,SelLit2,Body2),
	Nr \= Nr2,
	append([Left2,Body2,Right2],NewGoal2),
	live(NewGoal2).
	
/* --------------- */
/* has_matching_rule/2 */
/* --------------- */

/* tests wheter a given goal matches a rule (i.e. non-fact) for a given
	selected literal */

pre_condition(has_matching_rule(Goal,_NrOfSel)) :-
	term_is_of_type(Goal,goal).
post_condition(has_matching_rule(_Goal,NrOfSel)) :-
	term_is_of_type(NrOfSel,integer).

has_matching_rule(Goal,NrOfSel) :-
	copy(Goal,Goal1),
	split_list(Goal1,NrOfSel,Left,SelCall,Right),
	peel_off_calls(SelCall,SelLit),
	\+(is_negative_literal(SelLit,_NegAtom)),
	\+(pp_cll(is_built_in_literal(SelLit))),
	claus(_Nr,SelLit,[BodyHead|BodyRest]),
	append([BodyHead|BodyRest],Right,IntGoal),
	append(Left,IntGoal,NewGoal),
	live(NewGoal).

/* ---------------------- */
/* reduces_search_space/2 */
/* ---------------------- */

reduces_search_space(Goal,NrOfSel) :-
	copy(Goal,Goal1),
	split_list(Goal1,NrOfSel,Left,SelCall,Right),
	peel_off_calls(SelCall,SelLit),
	\+(is_negative_literal(SelLit,_NegAtom)),
	\+(pp_cll(is_built_in_literal(SelLit))),
	maximally_general_atom(SelLit,MaxLit),
	claus(Nr,MaxLit,_MB),
	\+(leads_to_live_resolvent(Nr,SelLit,Right,Left)).

leads_to_live_resolvent(Nr,SelLit,Right,Left) :-
	claus(Nr,SelLit,Body),
	append(Body,Right,IntGoal),
	append(Left,IntGoal,NewGoal),
	live(NewGoal).
	
maximally_general_goal([],[]).
maximally_general_goal([H|T],[MH|MT]) :-
	maximally_general_atom(H,MH),
	maximally_general_goal(T,MT).

maximally_general_atom(X,MX) :-
	nonvar(X),
	X =.. [Pred|Args],
	make_list_of_vars(Args,VarArgs),
	MX =..[Pred|VarArgs].

make_list_of_vars([],[]).
make_list_of_vars([_H|T],[_|VT]) :- make_list_of_vars(T,VT).
	


/* --------------------------- */
/* goal_increasing_selection/2 */
/* --------------------------- */

pre_condition(goal_increasing_selection(Goal,_NrOfSel)) :-
	term_is_of_type(Goal,goal).
post_condition(goal_increasing_selection(_Goal,NrOfSel)) :-
	term_is_of_type(NrOfSel,integer).

goal_increasing_selection(Goal,NrOfSel) :-
	copy(Goal,Goal1),
	split_list(Goal1,NrOfSel,_Left,SelCall,_Right),
	peel_off_calls(SelCall,SelLit),
	\+(is_negative_literal(SelLit,_NegAtom)),
	\+(pp_cll(is_built_in_literal(SelLit))),
	claus(_Nr,SelLit,Body),
	Body = [_Atom1,_Atom2|_RestBody],
	member_nr(A1,Body,Nr1),
	extract_positive_atom_from_literal(A1,_PA1),
	member_nr(A2,Body,Nr2),
	Nr1\==Nr2,
	extract_positive_atom_from_literal(A2,_PA2).
		/* clause with at least two atoms found
			-> unfolding will increase the size of the goal */
	


/* ================================================ */
/*          NEGATIVE and BUILT-IN LITERALS          */
/* ================================================ */

ecce_type(literal,nonvar).

/* --------------------------------- */
/* goal_contains_undefined_literal/1 */
/* --------------------------------- */

pre_condition(goal_contains_undefined_literal(Goal)) :-
	term_is_of_type(Goal,goal).
post_condition(goal_contains_undefined_literal(_Goal)).

goal_contains_undefined_literal([H|_T]) :-
	is_undefined_literal(H),!.
goal_contains_undefined_literal([_H|T]) :-
	goal_contains_undefined_literal(T).

/* --------------------- */
/* is_undefined_literal/1 */
/* --------------------- */

pre_condition(is_undefined_literal(Literal)) :-
	term_is_of_type(Literal,literal).
post_condition(is_undefined_literal(_Literal)).

is_undefined_literal(CLiteral) :-
	peel_off_calls(CLiteral,Literal),
    Literal \= fail,
	(is_open_literal(Literal)
	  -> true
	   ; \+(is_negative_literal(Literal,_Atom)),
	     \+(is_built_in_literal(Literal))
	),
	Literal =.. [Pred|Args],
	generate_variables(Args,Vars),
	Gen =.. [Pred|Vars],
	\+(claus(_Nr,Gen,_Body)).

generate_variables([],[]).
generate_variables([_H|T],[_Var|VT]) :-
	generate_variables(T,VT).

/* --------------------- */
/* is_negative_literal/2 */
/* --------------------- */

pre_condition(is_negative_literal(Literal,_Atom)) :-
	term_is_of_type(Literal,literal).
post_condition(is_negative_literal(_Literal,Atom)) :-
	term_is_of_type(Atom,nonvar).

is_negative_literal(Literal,Atom) :-
%	verify_pre(is_negative_literal(Literal,Atom)),
	peel_off_calls(Literal,PLiteral),
	(PLiteral = not(CAtom) ; PLiteral = \+(CAtom)),
	peel_off_calls(CAtom,Atom).

/* --------------------- */
/* is_built_in_literal/1 */
/* --------------------- */

pre_condition(is_built_in_literal(Literal)) :-
	term_is_of_type(Literal,literal).
post_condition(is_built_in_literal(_Literal)).

is_built_in_literal('!').
is_built_in_literal('='(_X,_Y)).
is_built_in_literal('C'(_X,_Y,_Z)).
is_built_in_literal(true).
is_built_in_literal(fail).
is_built_in_literal('=..'(_X,_Y)).
is_built_in_literal(functor(_X,_Y,_Z)).
is_built_in_literal(arg(_X,_Y,_Z)).
is_built_in_literal('is'(_X,_Y)).
is_built_in_literal('<'(_X,_Y)).
is_built_in_literal('=<'(_X,_Y)).
is_built_in_literal('>'(_X,_Y)).
is_built_in_literal('>='(_X,_Y)).
is_built_in_literal(call(X)) :- var(X).
is_built_in_literal(call(X)) :- nonvar(X), is_built_in_literal(X).
is_built_in_literal(nonvar(_X)).   /* declarative if delays until nonvar */
is_built_in_literal(ground(_X)).   /* declarative if delays until nonvar */
is_built_in_literal(number(_X)).   /* declarative if delays until nonvar */
is_built_in_literal(real(_X)).   /* declarative if delays until nonvar */
is_built_in_literal(integer(_X)).   /* declarative if delays until nonvar */
is_built_in_literal(atomic(_X)).   /* declarative if delays until nonvar */
is_built_in_literal(atom(_X)).   /* declarative if delays until nonvar */
is_built_in_literal('\\=='(_X,_Y)). /* declarative if delays until sufficiently ground */
is_built_in_literal('=='(_X,_Y)). /* declarative if delays until sufficiently ground */
is_built_in_literal('\\='(_X,_Y)). 
is_built_in_literal(dif(_X,_Y)). 
is_built_in_literal(clause(_X,_Y)). 
is_built_in_literal(print(_X)).   	/* not declarative */
is_built_in_literal(write(_X)).   	/* not declarative */
is_built_in_literal(read(_X)).   	/* not declarative */
is_built_in_literal(ecce_get(_X)).   	/* not declarative */
is_built_in_literal(nl). 		/* not declarative */
is_built_in_literal(assert(_X)). 	/* not declarative */
is_built_in_literal(asserta(_X)). 	/* not declarative */
is_built_in_literal(assertz(_X)). 	/* not declarative */
is_built_in_literal(retract(_X)). 	/* not declarative */
is_built_in_literal(var(_X)). 		/* not declarative */
is_built_in_literal(copy(_X,_Y)). 	/* not declarative */
is_built_in_literal(term_variables(_X,_Vs)). 	/* not declarative */
is_built_in_literal(numbervars(_X,_,_)). 	/* not declarative */
is_built_in_literal(ecce_call(_Cond,_Call)). 	/* FORCE ECCE TO CALL A PREDICATE */
is_built_in_literal(in(_CLPFDVariable,_Domain)) :- clpfd_active(yes).
is_built_in_literal('#<'(_FDT1,_FDT2)) :- clpfd_active(yes).
is_built_in_literal('#\\='(_FDT1,_FDT2)) :- clpfd_active(yes).
is_built_in_literal('#>'(_FDT1,_FDT2)) :- clpfd_active(yes).
is_built_in_literal('#='(_FDT1,_FDT2)) :- clpfd_active(yes).
is_built_in_literal(labeling(_Opts,_Vars)) :- clpfd_active(yes).
is_built_in_literal(X) :- is_constraint_literal(X).
is_built_in_literal(X) :-
	nonvar(X),
	is_open_literal(X).



is_constraint_literal(in(_CLPFDVariable,_Domain)) :- clpfd_active(yes).
is_constraint_literal('#<'(_FDT1,_FDT2)) :- clpfd_active(yes).
is_constraint_literal('#\\='(_FDT1,_FDT2)) :- clpfd_active(yes).
is_constraint_literal('#>'(_FDT1,_FDT2)) :- clpfd_active(yes).
is_constraint_literal('#='(_FDT1,_FDT2)) :- clpfd_active(yes).
is_constraint_literal(clpfd:'t=u+c'(_,_,_)) :- clpfd_active(yes).
is_constraint_literal(clpfd:'ax+y=t'(_,_,_,_)) :- clpfd_active(yes).

is_constraint_literal(rul__constraint__declaration(_C,_Def)) :-
		      rul_active(yes).


keep_constraint_literal_in_residual_program(in(_CLPFDVariable,_Domain)) :- 
     clpfd_active(yes).
keep_constraint_literal_in_residual_program('#<'(_FDT1,_FDT2)) :- clpfd_active(yes).
keep_constraint_literal_in_residual_program('#\\='(_FDT1,_FDT2)) :- 
     clpfd_active(yes).
keep_constraint_literal_in_residual_program('#>'(_FDT1,_FDT2)) :- clpfd_active(yes).
keep_constraint_literal_in_residual_program('#='(_FDT1,_FDT2)) :- clpfd_active(yes).
keep_constraint_literal_in_residual_program(clpfd:'t=u+c'(_,_,_)) :- 
     clpfd_active(yes).
keep_constraint_literal_in_residual_program(clpfd:'ax+y=t'(_,_,_,_)) :- 
     clpfd_active(yes).

/* ------------------------ */
/* divide_constraint_goal/3 */
/* ------------------------ */

pre_condition(divide_constraint_goal(Goal,_,_)) :-
	term_is_of_type(Goal,goal).
post_condition(divide_constraint_goal(_Goal,O,C)) :-
	term_is_of_type(O,goal),term_is_of_type(C,goal).

self_check(must_fail(divide_constraint_goal([a,q(_X,_Y)],[],_))).
self_check(must_succeed(divide_constraint_goal([q(X,Y)],[q(X,Y)],[]))).
self_check(must_succeed(divide_constraint_goal([a,q(X,Y)],[a,q(X,Y)],[]))).
self_check(must_succeed(divide_constraint_goal([],[],[]))).

divide_constraint_goal([],[],[]).
divide_constraint_goal([Lit|T],OrdLits,ConstrLits) :-
   (is_constraint_literal(Lit)
    -> ConstrLits = [Lit|C2], O2 = OrdLits
    ;  OrdLits = [Lit|O2], C2 = ConstrLits
   ),
   divide_constraint_goal(T,O2,C2).

/* ------------------------ */
/* divide_constraint_residual_goal/3 */
/* ------------------------ */

pre_condition(divide_constraint_residual_goal(Goal,_,_)) :-
	term_is_of_type(Goal,goal).
post_condition(divide_constraint_residual_goal(_Goal,O,C)) :-
	term_is_of_type(O,goal),term_is_of_type(C,goal).

self_check(must_fail(divide_constraint_residual_goal([a,q(_X,_Y)],[],_))).
self_check(must_succeed(divide_constraint_residual_goal([q(X,Y)],[q(X,Y)],[]))).
self_check(must_succeed(divide_constraint_residual_goal([a,q(X,Y)],[a,q(X,Y)],[]))).
self_check(must_succeed(divide_constraint_residual_goal([],[],[]))).

divide_constraint_residual_goal([],[],[]).
divide_constraint_residual_goal([Lit|T],OrdLits,ConstrLits) :-
   (is_constraint_literal(Lit)
     ->(keep_constraint_literal_in_residual_program(Lit)
         -> ConstrLits = [Lit|C2], O2 = OrdLits
         ;  ConstrLits = C2, OrdLits = O2
       )
    ;  OrdLits = [Lit|O2], C2 = ConstrLits
   ),
   divide_constraint_residual_goal(T,O2,C2).

/* --------------- */
/* call_built_in/1 */
/* --------------- */

call_built_in(X) :- var(X), print('### ERROR call(_)'),!,fail.
call_built_in('=..'(X,Y)) :- nonvar(Y), Y = [A|T],
 nonvar(T), T = [], !, debug_print('=..'),X = A.
call_built_in(call(X)) :- !,call_built_in(X).
call_built_in(ecce_call(_Cond,X)) :- !,ecce_call(X).
call_built_in('is'(X,_ArithExpr)) :-
	nonvar(X), \+(number(X)),!,print('::='),fail.
call_built_in(BI) :- debug_print(call_bi(BI)),
	on_exception(Exc,call(BI),
	             (nl,print('! ERROR: Exception occured while evaluating built-in'),nl,
	                 print('!        Call: '), print(BI),nl,
	                 print('!        Exception: '), print(Exc),nl,nl,
	                 fail)).
	/* (call(BI) -> (print(yes(BI)),nl) ; (print(no),nl,fail)). */

ecce_call(X) :- print(ecce_call(X)),call(X),print('@'),nl.

/* ------------------------------ */
/* is_callable_built_in_literal/1 */
/* ------------------------------ */

pre_condition(is_callable_built_in_literal(Literal)) :-
	term_is_of_type(Literal,literal).
post_condition(is_callable_built_in_literal(_Literal)).

is_callable_built_in_literal('='(_X,_Y)).
is_callable_built_in_literal('C'(_X,_Y,_Z)).
is_callable_built_in_literal(true).
is_callable_built_in_literal(fail).
is_callable_built_in_literal('=..'(X,Y)) :-
	(nonvar(X)) ; (list_ok_for_eqdotdot(Y)).
is_callable_built_in_literal(functor(X,Y,Z)) :-
	(nonvar(X)) ; (integer(Y),atom(Z)).
is_callable_built_in_literal(arg(X,Y,_Z)) :-
	nonvar(Y),integer(X).
is_callable_built_in_literal('is'(_X,Y)) :-
	ground(Y).
is_callable_built_in_literal('<'(X,Y)) :-
	ground(X),ground(Y).
is_callable_built_in_literal('=<'(X,Y)) :-
	ground(X),ground(Y).
is_callable_built_in_literal('>'(X,Y)) :-
	ground(X),ground(Y).
is_callable_built_in_literal('>='(X,Y)) :-
	ground(X),ground(Y).
is_callable_built_in_literal(nonvar(X)) :-
	nonvar(X).
is_callable_built_in_literal(ground(X)) :-
	ground(X).
is_callable_built_in_literal(number(X)) :-
	nonvar(X).
is_callable_built_in_literal(real(X)) :-
	nonvar(X).
is_callable_built_in_literal(integer(X)) :-
	nonvar(X).
is_callable_built_in_literal(atomic(X)) :-
	nonvar(X).
is_callable_built_in_literal(atom(X)) :-
	nonvar(X).
is_callable_built_in_literal(call(X)) :-
	nonvar(X),
	is_callable_built_in_literal(X).
is_callable_built_in_literal(clause(X,_Y)) :-
	nonvar(X).
is_callable_built_in_literal('\\=='(X,Y)) :-
	(X\=Y ; (ground(X),ground(Y)) ; (X==Y) ).
is_callable_built_in_literal('=='(X,Y)) :-
	(X\=Y ; (ground(X),ground(Y)) ; (X==Y) ).
is_callable_built_in_literal('\\='(X,Y)) :-
	(X\=Y ; (ground(X),ground(Y)) ; (X==Y) ). 
is_callable_built_in_literal(dif(X,Y)) :-
	(X\=Y ; (ground(X),ground(Y)) ; (X==Y) ). 
is_callable_built_in_literal(print(_X)) :-
	fail.
is_callable_built_in_literal(nl) :-
	fail.
is_callable_built_in_literal(term_variables(X,_Vs)) :-
	ground(X).
is_callable_built_in_literal(numbervars(X,_,_)) :-
	ground(X).
is_callable_built_in_literal(ecce_call(Condition,_X)) :-
    user_expert(yes),
    call(Condition).

list_ok_for_eqdotdot(L) :-
	nonvar(L), L = [_A|T], nonvar(T), T=[], !.
/* atomic(A) not required as built-in executed by ECCE */
list_ok_for_eqdotdot(L) :-
	nonvar(L),L = [H|T],
	atom(H),
	terminated_list(T).

terminated_list(X) :- nonvar(X),term_list2(X).
term_list2([]).
term_list2([_H|T]) :- terminated_list(T).

/* ----------------------------- */
/* built_in_generates_bindings/1 */
/* ----------------------------- */

pre_condition(built_in_generates_bindings(Literal)) :-
	term_is_of_type(Literal,literal).
post_condition(built_in_generates_bindings(_Literal)).

built_in_generates_bindings(X) :- var(X),!,
	print('### Warning: variable arg for built_in_generates_bindings/1'),nl.
built_in_generates_bindings('='(_X,_Y)).
built_in_generates_bindings('=..'(_X,_Y)).
built_in_generates_bindings(functor(_X,_Y,_Z)).
built_in_generates_bindings(arg(_X,_Y,_Z)).
built_in_generates_bindings('is'(_X,_Y)).
built_in_generates_bindings(call(X)) :- built_in_generates_bindings(X).
built_in_generates_bindings(term_variables(_X,_Vs)).
built_in_generates_bindings(numbervars(_X,_,_)).
built_in_generates_bindings(X) :- is_constraint_literal(X).


/* --------------------------- */
/* simplify_built_in_literal/2 */
/* --------------------------- */

pre_condition(simplify_built_in_literal(Literal,_SLit)) :-
	term_is_of_type(Literal,literal).
post_condition(simplify_built_in_literal(_Literal,SLit)) :-
	term_is_of_type(SLit,literal).


simplify_built_in_literal(call(X),X) :-
	nonvar(X),!.

/* ========== */
/* PREDICATES */
/* ========== */

/* --------------- */
/* get_predicate/2 */
/* --------------- */

ecce_type(predicate,term(pred,[ground,arity])).
ecce_type(arity,integer).

get_predicate(Atom,pred(PredName,Arity)) :-
	functor(Atom,PredName,Arity).
	
	
get_predicate_through_calls(call(X),Pred) :- nonvar(X),!,
   get_predicate_through_calls(X,Pred).
get_predicate_through_calls(Atom,Pred) :-
  get_predicate(Atom,Pred).

/* --------------------------- */
/* atoms_have_same_predicate/3 */
/* --------------------------- */

atoms_have_same_predicate(A,B,Pred) :-
	get_predicate(A,Pred),
	get_predicate(B,Pred).

/* ------------------ */
/* msg_can_be_taken/2 */
/* ------------------ */

pre_condition(msg_can_be_taken(Goal1,Goal2)) :-
	term_is_of_type(Goal1,goal),
	term_is_of_type(Goal2,goal).
post_condition(msg_can_be_taken(_Goal1,_Goal2)).

msg_can_be_taken([],[]).
msg_can_be_taken([A1|T1],[A2|T2]) :-
	atoms_have_same_predicate(A1,A2,_Pred),
	msg_can_be_taken(T1,T2).


/* ----------------- */
/* print_predicate/1 */
/* ----------------- */

print_predicate(Pred) :-
	(Pred=pred(Name,Arity)
	-> print(Name),print('/'),print(Arity)
	;  print('### not pred(Name,Arity): '),print(Pred)
	).

/* --------- */
/* sharing/2 */
/* --------- */

pre_condition(sharing(Goal1,Goal2)) :-
	term_is_of_type(Goal1,goal),
	term_is_of_type(Goal2,goal).
post_condition(sharing(_Goal1,_Goal2)).

sharing(Goal1,Goal2) :-
	varlist(Goal1,Vars1),
	not(not(share_vars(Goal2,Vars1))).

share_vars(X,Vars) :-
	numbervars(X,1,_),
	contains_nonvar(Vars).

contains_nonvar([X|_T]) :- nonvar(X).
contains_nonvar([_X|T]) :- contains_nonvar(T).

/* --------- */
/* SUB GOALS */
/* --------- */


connected_sub_goal([],Goal,[]) :- not(Goal= []).
connected_sub_goal([X|T],InGoal,[X|ConSubGoal]) :-
	var_can_be_added_to_subgoal(X,InGoal),
	connected_sub_goal(T,[X|InGoal],ConSubGoal).
connected_sub_goal([_X|T],InGoal,ConSubGoal) :-
	connected_sub_goal(T,InGoal,ConSubGoal).


sub_goal([],[]).
sub_goal([X|T],Res) :-
	sub_goal(T,ST),
	(ST\=[], Res = ST ; Res = [X|ST]).

var_can_be_added_to_subgoal(X,InGoal) :-
	(sharing(X,InGoal) ; InGoal = []),!.




/* ============================================================ */
/*                   GETTING THE LEAVES                         */
/* ============================================================ */

/* ++++++++++++++++ */
/* TYPE DEFINITIONS */
/* ++++++++++++++++ */
ecce_type(resultant_nr,integer).
ecce_type(resultant_nr,term(abstracted,[])).
ecce_type(chposition,term(chpos,[resultant_nr,split_indication])).
ecce_type(chposition,term(local_root,[])).
ecce_type(split_goal,term(split_goal,[goal,split_indication])).
ecce_type(split_indication,list(selected_literal_nr)).
ecce_type(split_indication,term(neg,[selected_literal_nr])).
ecce_type(split_indication,term(built_in,[selected_literal_nr])).
/* ++++++++++++++++ */


get_leaf(Chtree,Goal,Leaf,Chposition) :-
	reset_gennum(1),
	pp_cll(leaf(Chtree,Goal,Leaf,Chposition)).

/* ------ */
/* leaf/4 */
/* ------ */

pre_condition(leaf(Chtree,Goal,_Leaf,_Chposition)) :-
	term_is_of_type(Chtree,chtree),
	term_is_of_type(Goal,goal).
post_condition(leaf(_Chtree,_Goal,Leaf,Chposition)) :-
	term_is_of_type(Leaf,goal),
	term_is_of_type(Chposition,chposition).


leaf(empty,_Goal,_Leaf,_ChPos) :- fail.
leaf(success,_Goal,_Leaf,_ChPos) :- fail. /* no leaves */
leaf(stop,Goal,Leaf,chpos(ResultantNr,SplitIndication)) :-
	gennum(ResultantNr),
	pp_cll(more_specific_transformation(Goal)),
	pp_mnf(partition_goal(Goal,SplittedGoals)),
	member(split_goal(Leaf,SplitIndication),SplittedGoals).
leaf(select(SelLitNr,Chpaths),Goal,Leaf,ChPos) :-
	pp_cll(more_specific_transformation(Goal)),
	pp_mnf(split_list(Goal,SelLitNr,Left,SelCall,Right)),
	peel_off_calls(SelCall,Sel),
	member(match(ClauseNr,SubTree),Chpaths),
	(claus(ClauseNr,Sel,Body) -> true
		; (print('### Error: clause not matching in leaf/4'),nl,
		   print('###  ClauseNr:'),print(ClauseNr),nl,
		   print('###  SelAtom: '),print(Sel),nl,fail)
	),
	pp_mnf(append(Body,Right,IntGoal)),
	pp_mnf(append(Left,IntGoal,NewGoal)),
	pp_cll(leaf(SubTree,NewGoal,Leaf,ChPos)).
leaf(remove(SelLitNr,_Predicate,SubTree),Goal,Leaf,ChPos) :-
	pp_cll(more_specific_transformation(Goal)),
	pp_mnf(split_list(Goal,SelLitNr,Left,_Sel,Right)),
/*	(is_negative_literal(Sel,NegAtom),
	 get_predicate(NegAtom,Predicate) -> (true)
		; (print('### Error: illegal negative literal in leaf/4'),nl,
		   print('###  '),print(Sel),print(' is not: '),
		   print_predicate(Predicate),nl)
	), */
	pp_mnf(append(Left,Right,NewGoal)),
	pp_cll(leaf(SubTree,NewGoal,Leaf,ChPos)).
leaf(built_in_eval(SelLitNr,BI,SubTree),Goal,Leaf,ChPos) :-
	pp_cll(more_specific_transformation(Goal)),
	pp_mnf(split_list(Goal,SelLitNr,Left,Sel,Right)),
	(get_predicate_through_calls(Sel,BI) -> true
	 ;  (print('### Warning: illegal built-in in leaf/4'),nl,
	     print('###  '),print(Sel), print(' is not of type:'),
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
	pp_cll(leaf(SubTree,NewGoal,Leaf,ChPos)).

extract_positive_atom_from_literal(X,Atom) :-
	var(X),!, Atom = call(X).
/*	print('/'),print('* '),
	print('### Warning: Variable as a call in a literal !'),nl,
	print('   ### Keep Original Program for Correctness !'),nl,
	print(' *'),print('/'),nl. */
extract_positive_atom_from_literal(not(X),Atom) :- !,
	extract_positive_atom_from_literal(X,Atom).
extract_positive_atom_from_literal(\+(X),Atom) :- !,
	extract_positive_atom_from_literal(X,Atom).
extract_positive_atom_from_literal(call(X),Atom) :- !,
	extract_positive_atom_from_literal(X,Atom).
extract_positive_atom_from_literal(Atom,Atom) :-
	not(pp_cll(is_built_in_literal(Atom))).


l_peel_off_calls([],[]).
l_peel_off_calls([CC|CT],[C|T]) :-
	peel_off_calls(CC,C),
	l_peel_off_calls(CT,T).

peel_off_calls(X,X) :- var(X),!.
peel_off_calls(call(X),Y) :- nonvar(X),!, peel_off_calls(X,Y).
peel_off_calls(X,X).


/* ---------------- */
/* partition_goal/2 */
/* ---------------- */

pre_condition(partition_goal(Goal,_SplittedGoals)) :-
	term_is_of_type(Goal,goal).
post_condition(partition_goal(_Goal,SplittedGoals)) :-
	term_is_of_type(SplittedGoals,list(split_goal)).

partition_goal([],[]) :- !.
partition_goal(UPGoal,SplittedGoal) :-
	l_peel_off_calls(UPGoal,Goal),
	get_literal_numbers(Goal,1,Nrs),
	(find_any_unimposed_variant(Goal,_VariantGoalID)
	 -> (/* do not partition then */
	     SplittedGoal = [split_goal(Goal,Nrs)]
	    )
	 ;  (pp_cll(partition_goal(Goal,Nrs,SplittedGoal)))
	),
	debug_print(partition_goal(Goal,SplittedGoal)),debug_nl.


get_literal_numbers([],_Nr,[]).
get_literal_numbers([_H|T],Nr,[Nr|TN]) :-
	Nr1 is Nr + 1,
	get_literal_numbers(T,Nr1,TN).


/* ========================================================== */
/*     PRE-  AND  POST-CONDITIONS FOR PARAMETRIC  PREDICATES  */
/* ========================================================== */

pre_condition(partition_goal(Goal,LiteralNrs,_SplittedGoals)) :-
	term_is_of_type(Goal,goal),
	term_is_of_type(LiteralNrs,list(integer)).
post_condition(partition_goal(_Goal,_LiteralNrs,SplittedGoals)) :-
	term_is_of_type(SplittedGoals,list(split_goal)).


pre_condition(select_positive_literal(Goal,TopGoalVarlist,
					UnfHist,_NrOfSelLiteral,_SelLit)) :-
	term_is_of_type(Goal,goal),
	term_is_of_type(TopGoalVarlist,list(any)),
	term_is_of_type(UnfHist,unfold_history).
post_condition(select_positive_literal(_Goal,TopGoalVarlist,
					_UnfHist,NrOfSelLiteral,SelLit)) :-
	term_is_of_type(NrOfSelLiteral,selected_literal_nr),
	term_is_of_type(TopGoalVarlist,list(any)),
	term_is_of_type(SelLit,literal),
	not(is_negative_literal(SelLit,_NA)),
	not(pp_cll(is_built_in_literal(SelLit))).


pre_condition(more_specific_transformation(Goal)) :-
	term_is_of_type(Goal,goal).
post_condition(more_specific_transformation(Goal)) :-
	term_is_of_type(Goal,goal).


pre_condition(post_prune_chtree(Goal,Chtree,_PrunedChtree)) :-
	term_is_of_type(Goal,goal),
	term_is_of_type(Chtree,chtree).
post_condition(post_prune_chtree(_Goal,_Chtree,PrunedChtree)) :-
	term_is_of_type(PrunedChtree,chtree).


ecce_type(neg_solve_solution,term(success,[])).
ecce_type(neg_solve_solution,term(fail,[])).
ecce_type(neg_solve_solution,term(unknown,[])).

pre_condition(neg_solve(Goal,_Solution)) :-
	term_is_of_type(Goal,goal).
post_condition(neg_solve(_Goal,Solution)) :-
	term_is_of_type(Solution,neg_solve_solution).
