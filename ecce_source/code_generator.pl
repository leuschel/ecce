:- module(code_generator,
	[
	    spec_clause/3,
	    cg_filter_goal/4,
	    cg_filter_goal/3,
	    filter_top_level_call/2,
	    generate_code/0,
	    reset_spec_prog/0,
	    copy_specialised_program_to_input/0,
	    init_filters/0,
	    filter_and_rename_predicates/0,
	    generate_new_filters/0,
	    code_has_to_be_generated_for_node/1,
	    generate_resultants/0,
	    resultants_generated/1,
	    generate_resultants_for_goal/3,
	    assert_spec_clause/2,
	    assert_unsimplified_spec_clause/2,
	    simplify_calls/2,
	    print_specialised_program/0,
	    wrap_in_calls/2,
	    variant_clause_already_exists/2,
	    useless_clause/2,
	    is_list_of_free_variables/1,
	    add_used_clause/1,
	    reset_used_clause/0,
	    get_slice_to_remove/3,
	    tcltk_get_slices_to_remove/2,
	    tcltk_print_sliced_line_numbers/1,
	    extract_reverse/5,
	    get_end_of_layout/2,
	    resultant_body/4,
	    rename_resultant_goal/4,
	    make_conjunction/2,
	    split_according_to_split_indication/7,
	    literal_covered_by_split_indication/2,
	    cg_extract_positive_atom_from_literal/4,
	    get_filtered_version/3,
	    get_filtered_goal_id/2,
	    print_clause_with_nl/2,
	    remove_redundant_calls/3,
	    redundant_call/2,
	    call_exists_in_list/2,
	    print_body_with_nl/1,
	    print_body/1,
	    print_call/1,
	    print_atom/1,
	    print_faithful_functor/1,
	    infix_predicate/1,
	    simple_assert_spec_clause/2
	]).


:- use_package( .(ecce_no_rt) ).


/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,1996,1997 */
/* --------------------------------------------- */

/* TO GET THE LATEST VERSION OF THE ECCE SYSTEM GO TO:
	http://www.ecs.soton.ac.uk/~michael */
/* BUG REPORTS, QUESTIONS AND COMMENTS CAN BE SENT TO:
	mal@ecs.soton.ac.uk */

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).


/* file: code_generator.pro */

/*
:- ensure_consulted('$BIMTOOLS_PATH/gensym.pro').
:- ensure_consulted('$BIMTOOLS_PATH/StdLists.pro').
:- ensure_consulted('$BIMTOOLS_PATH/typechecker.pro').
:- ensure_consulted('$BIMTOOLS_PATH/prepost.pro').
*/
:- use_module(bimtools).

:- use_module(dynpreds).
:- use_module(global_tree).
:- use_module(calc_chtree).
%:- use_module('rul/ecceRUL').
%:- use_module('constraints/constraints_clpfd').
:- use_module('more_specific/more_specific').

:- use_module(main_functions).

:- include( multi_meta ).

:- dynamic spec_clause/3.

:- dynamic cg_filter_goal/4.

cg_filter_goal(ID,MSVCall,FiltCall) :-
  cg_filter_goal(ID,_OrigCall,MSVCall,FiltCall).


/* ----------------------- */
/* filter_top_level_call/2 */
/* ----------------------- */

/* only run after generate_code has been run */


pre_condition(filter_top_level_call(OrigCall,_FCall)) :-
	term_is_of_type(OrigCall,goal).
post_condition(filter_top_level_call(_OrigCall,FCalls)) :-
	term_is_of_type(FCalls,goal).

filter_top_level_call(OrigCall,FCalls) :-
	gt_node_descends_from(RootNodeID,root,_ChPos),
	get_filtered_version(OrigCall,RootNodeID,FCalls).
	/* cg_filter_goal(RootNodeID,OrigCall,FCall).*/
	

/* --------------- */
/* generate_code/0 */
/* --------------- */

generate_code :-
	filter_and_rename_predicates,!,
	reset_spec_prog,!,
	generate_resultants,
	(generate_slice_instead_of_spec_prog(yes)
	   -> generate_slice ; true).  /* added 20/5/04 */



reset_spec_prog :-
	retract(spec_clause(_Nr,_Head,_Body)),fail.
reset_spec_prog.


copy_specialised_program_to_input :-
	spec_clause(Nr,Head,Body),
	(Nr\=filter_comment),
	(Nr\=keep_original_program),
	add_new_clause(Head,Body),
	fail.
copy_specialised_program_to_input :-
    generate_slice_instead_of_spec_prog(no),
	cg_filter_goal(_NodeID,_Goal,_MsvGoal,FGoal),
	\+(spec_clause(_SpecClauseNr,FGoal,_Body)),
	add_new_clause(FGoal,[fail]),
	fail.
copy_specialised_program_to_input.

init_filters :-
	retract(cg_filter_goal(_ID,_Goal,_MsvGoal,_FGoal)),fail.
init_filters.

/* ------------------------------ */
/* filter_and_rename_predicates/0 */
/* ------------------------------ */


filter_and_rename_predicates :-
	init_filters,
	reset_gennum(1),
	generate_new_filters.


generate_new_filters :-
	gt_node(NodeID),
	code_has_to_be_generated_for_node(NodeID),
	gt_node_goal(NodeID,Goal),
        gt_node_bup_cas(NodeID,(Goal,_),(MsvGoal,_)), 
                   /* instantiate using bup most specific version */
	gennum(Nr),
	debug_print(generating_filter(NodeID,Nr,Goal)),debug_nl,
	pp_mnf(filter_goal(MsvGoal,Nr,FAtom)),
	assertz(cg_filter_goal(NodeID,Goal,MsvGoal,FAtom)),fail.
generate_new_filters.


/* ----------------------------------- */
/* code_has_to_be_generated_for_node/1 */
/* ----------------------------------- */

code_has_to_be_generated_for_node(NodeID) :-
	\+(gt_node_instance_of(NodeID,_InstanceOfID)),
	gt_node_pe_status(NodeID,pe(_Imp)).


/* --------------------- */
/* generate_resultants/0 */
/* --------------------- */

generate_resultants :-
	nl,
	reset_used_clause,
	abnormal_goal_encountered(no),
		/* otherwise original program is kept */
	top_level_pegoal(TopPeGoal),
	/* create an unfiltered version for the top-level query as well */
	TopPeGoal = [Atom], /* do it only for atoms */
	pp_mnf(filter_top_level_call(TopPeGoal,FCalls)),
	simple_assert_spec_clause(Atom,FCalls),fail.
generate_resultants :-
	cg_filter_goal(NodeID,_Goal,MsvGoal,FGoal),
	generate_resultants_for_goal(NodeID,MsvGoal,FGoal),
	fail.
generate_resultants.

simple_assert_spec_clause(Head,Body) :-
    gensym(spec,SpecClauseNr),
	assertz(spec_clause(SpecClauseNr,Head,Body)).


:- dynamic resultants_generated/1.
resultants_generated(0).

generate_resultants_for_goal(NodeID,_Goal,_FGoal) :-
	retract(resultants_generated(NodeID)),fail.
generate_resultants_for_goal(NodeID,Goal,FGoal) :-
	gt_node_chtree(NodeID,Chtree),
	debug_print(generate_resultants_for_goal(NodeID)),debug_nl,
        gt_node_bup_cas(NodeID,(_,_),(_,BUPC)),
        (abstract_partial_deduction_enabled(no) ; \+(BUPC=fail)),
	assertz(spec_clause(filter_comment,FGoal,Goal)),
	(Chtree\=stop
	 ->  reset_gennum(1),
	     debug_print(calling_resultant_body(Goal)),debug_nl,
	     pp_cll(resultant_body(Chtree,NodeID,Goal,Body))
	 ;   assertz(spec_clause(keep_original_program,FGoal,Goal)),
	     wrap_in_calls(Goal,Body)
	),
	(detect_dead_literals_or_non_leftmost_builtins(no)
	   -> Body \= [fail|_] ; \+(member(fail,Body))),
	assert_spec_clause(FGoal,Body),
	debug_print(done_assert_spec_clause(FGoal,Body)),debug_nl,
	(resultants_generated(NodeID)
	   -> true ; assert(resultants_generated(NodeID))),
	fail.
generate_resultants_for_goal(NodeID,_Goal,FGoal) :-
	(resultants_generated(NodeID)
	 -> true
	 ;  (gensym(spec,SpecClauseNr),
	     assertz(spec_clause(SpecClauseNr,FGoal,[fail]))
	    )
	).


:- use_module('constraints/constraints_clpfd',[project_and_check_constraint/3]).
assert_spec_clause(Head,Body) :-
	(constraints_active
	 -> (debug_print(call_assert_spec_clause(Head,Body)),debug_nl,
             divide_constraint_goal(Body,OBody,Constraints),
             divide_constraint_residual_goal(Body,OBody,ResConstraints),
             debug_print(divide(OBody,Constraints,ResConstraints)),debug_nl,
             (Constraints=[]
              -> ResBody=OBody
              ;  (debug_print(checking(Constraints)),debug_nl,
	         project_and_check_constraint((Head,OBody),Constraints,_),
                 debug_print(checked(Constraints)),debug_nl,
                 project_and_check_constraint((Head,OBody),ResConstraints,ProjC),
	          debug_print(proj((Head,OBody),Constraints,ProjC)),debug_nl,
	          append(ProjC,OBody,ResBody)
                 )
             ),
	     debug_print(clpfd_spec_clause(Head,ResBody)),debug_nl
	    )
	 ;  ResBody = Body
	),
	remove_redundant_calls(ResBody,Body2,[]),
	simplify_calls(Body2,NewBody),
	((allow_removal_of_duplicate_predicates(no);
          \+(useless_clause(Head,NewBody)))
	 -> assert_unsimplified_spec_clause(Head,NewBody)
	 ;  true
	),!.

assert_unsimplified_spec_clause(Head,Body) :-
	\+(variant_clause_already_exists(Head,Body)),
	gensym(spec,SpecClauseNr),
	assertz(spec_clause(SpecClauseNr,Head,Body)),
	debug_print(assertz(spec_clause(SpecClauseNr,Head,Body))),debug_nl.

simplify_calls([],[]).
simplify_calls([H|T],SHT) :-
	simplify_call(H,SH),
	(SH=true -> SHT = ST ; SHT = [SH|ST]),
	simplify_calls(T,ST).

simplify_call(call(X),S) :-
	nonvar(X),!,simplify_call(X,S).
simplify_call(not(X),Res) :- !,
	(simplify_call(X,S)
	 -> (S=true -> fail ; Res = not(S))
	 ;  Res = true
	).
simplify_call(\+(X),Res) :- !,
	(simplify_call(X,S)
	 -> (S=true -> fail ; Res = \+(S))
	 ;  Res = true
	).
simplify_call(BI,S) :- detect_dead_literals_or_non_leftmost_builtins(yes),
	is_callable_built_in_literal(BI),!,
	call_built_in(BI), S = true.
/* e.g. add something like:
simplify_call(Call,S) :- Call == 'is'(Z,'+'(X,0)), !, X=Z, S = true.
*/
simplify_call(X,X).

/* --------------------------- */
/* print_specialised_program/0 */
/* --------------------------- */

% call if you want to get the specialised goals in sorted order
get_specialised_goal_in_order(Goal,MsvGoal,FilteredGoal) :-
  findall(g(G,M,F),cg_filter_goal(_NodeID,G,M,F),List),
  sort(List,SL),
  member(g(Goal,MsvGoal,FilteredGoal),SL).

print_specialised_program :-
	newparagraph,print('/'),print('* Specialised Predicates: '),nl,
    generate_slice_instead_of_spec_prog(no), /* only print spec preds if no slicing */
	print_html('<OL> '),
	get_specialised_goal_in_order(Goal,MsvGoal,FGoal),
        (variant_of(MsvGoal,Goal) -> Var=yes ; Var=no ),
	    numbervars(FGoal,0,NrOfVars),
	    N2 is NrOfVars + 26,
	    numbervars(Goal,N2,_), /* these are redundant arguments */
	    print_html('<LI> '),
	    filter_print_atom(FGoal),
	    print_body(MsvGoal),
            (Var=yes -> true
             ; print(' MSV of '), print_body(Goal)
            ), nl,
	fail.
print_specialised_program :-
	print_html(' </OL> '),
	print('*'),print('/'),newparagraph,nl,
	spec_clause(SpecClauseNr,FGoal,Body),
	(SpecClauseNr = filter_comment
	-> (nl,print('/'),print('* '),
	    numbervars(g(FGoal,Body),0,_),
	    print_em(FGoal), print_em(' --> '),
	    print_em(Body),print(' *'),print('/'),newparagraph
	   )
	;  (SpecClauseNr = keep_original_program
		-> (print('/'),print('* '),
	    	    print_bold(' KEEP ORIGINAL PROGRAM FOR CORRECTNESS !! '),
	     	    print(' *'),print('/'),newparagraph)
		;  print_clause_with_nl(FGoal,Body)
	   )
	),
	fail.
print_specialised_program :-
    generate_slice_instead_of_spec_prog(no), /* only print failing preds if no slicing */
	cg_filter_goal(_NodeID,_Goal,_MsvGoal,FGoal),
	\+(spec_clause(_SpecClauseNr,FGoal,_Body)),
	print_clause_with_nl(FGoal,[fail]),
	fail.
print_specialised_program.


wrap_in_calls(X,[call(uNkNoWn)]) :- var(X),!.
wrap_in_calls([],[]).
wrap_in_calls([H|T],[CH|CT]) :-
	(var(H) -> (CH = call(H)) ; (CH = H)),
	wrap_in_calls(T,CT).

variant_clause_already_exists(Head,Body) :-
	copy(c(Head,Body),c(H,B)),
	spec_clause(Nr,H,B), /* lookup potential match */
	spec_clause(Nr,SH,SB),
	variant_of(c(Head,Body),c(SH,SB)),!,
	debug_print(variant_clause_already_exists(Nr,Head,Body)),debug_nl.

useless_clause(Head,Body) :-
	Body = [Atom],
	Head =.. [_Pred|Args],
	is_list_of_free_variables(Args),
	Head == Atom,
	debug_print(useless_clause(Head,Body)),debug_nl.
		/* clauses of the form p(X) :- p(X). are useless */

/* Clauses of the form  H :- ...,H,... . are also useless in
 the sense that, for definite programs, they can only provide
 more instantiated answers. In the context of floundering they
 might give additional answers ! */



is_list_of_free_variables(X) :- var(X),!,fail.
is_list_of_free_variables([]).
is_list_of_free_variables([H|T]) :-
	var(H),
	is_list_of_free_variables(T).
	

/* ----------------------- */
/*         SLICING         */
/* ----------------------- */

:- dynamic used_clause/1.
:- dynamic unused_clause/3.

:- dynamic removed_predicate/1. /* predicate removed, eg. because selected inside negation */

/* used to keep track of which clauses are used in the original
 program, in order to generate a slice */
 

add_used_clause(X) :- used_clause(X)
    -> true ; assert(used_clause(X)).
reset_used_clause :- % nl,print(resetting_used_clauses),nl,nl,
   retractall(used_clause(_)),
   retractall(unused_clause(_,_,_)),
   retractall(removed_predicate(_)).
   
assert_removed_predicate(X) :- removed_predicate(X)
    -> true ; assert(removed_predicate(X)).

generate_slice :- 
   reset_spec_prog,
   print(' Generating Slice:'),nl,	
	claus(Nr,Head,Body), %print(cl(Nr,Head,Body)),nl,
	(used_clause(Nr)
	  -> assertz(spec_clause(Nr,Head,Body))
	  ;  (get_clause_line_numbers_for_slice(Nr,FirstLine,LastLine),
	      print(removed(Nr)),nl, %,FirstLine,LastLine)),nl,
	      assertz(unused_clause(Nr,FirstLine,LastLine)) )),
    fail.
generate_slice :- print('   generating failure clauses'),nl,
    removed_predicate(Pred),
    get_predicate(Call,Pred),
    claus(_,Call,_),
    get_predicate(Call2,Pred),
    \+(spec_clause(_,Call2,_)),
    print(adding_failure_clause(Call2)),nl,
    simple_assert_spec_clause(Call2,[fail]),
    fail.
generate_slice :-  print('  printing slice'),nl,print_slice,nl.

/* claus/3 --> claus/4! (claus_layout/2 ?) */

get_clause_line_numbers_for_slice(Nr,RealFirstLine,LastLine) :-
  get_clause_line_numbers(Nr,FirstLine,LastLine),
  N1 is Nr-1,
  ((LastLine>FirstLine,get_clause_line_numbers(Nr1,_,FirstLine))
    -> RealFirstLine is FirstLine+1 /* there was some comment which we must have read */
    ;  RealFirstLine = FirstLine
   ).

get_clause_line_numbers(Nr,FirstLine,LastLine) :-
    claus_layout(Nr,Layout), %print(cl_layout(Nr,Layout)),nl,
	(Layout = [X,Y]
	  -> (FirstLine=X,LastLine=Y)
	  ; (print('### cannot parse layout info: '),
	     print(claus_layout(Nr,Layout)),nl,
	     FirstLine = 0, LastLine = -1)).

print_slice :-
    print(' SLICE OF THE ORIGINAL PROGRAM: '),nl,
    print(' only works if all post-processing has been turned off or if original is reloaded'),
    nl,nl,
	claus(Nr,Head,Body),
	get_clause_line_numbers_for_slice(Nr,FirstLine,LastLine),
    print(Nr),print(': '), print(FirstLine),print('-'),print(LastLine),print(':'),
	(used_clause(Nr) ->
	  (nl,print_clause_with_nl(Head,Body))
	  ;  (print(' REMOVED '),nl)),
    fail.
print_slice :- removed_predicate(Pred),
    get_predicate(Call,Pred),
    \+(spec_clause(_,Call,_)),
    print_clause_with_nl(Call,[fail]),
    fail.
print_slice.

get_slice_to_remove(Nr,FirstLine,LastLine) :-
    claus(Nr,Head,Body),\+(used_clause(Nr)),
    get_clause_line_numbers(Nr,FirstLine,LastLine).
	
tcltk_get_slices_to_remove(BegList,EndList) :-
  findall((F,L), get_slice_to_remove(_,F,L), FLList),
  extract_reverse(FLList,[],BegList,[],EndList).

tcltk_print_sliced_line_numbers(File) :- print('% Outputting slice info to: '),
  print(File),nl,
  tell(File),!,
  tcltk_print_sliced_line_numbers,
  told,
  print('% done'),nl.
tcltk_print_sliced_line_numbers(File) :-
  print('Unable to open file for slice output: '), print(File),nl.
  
tcltk_print_sliced_line_numbers :-
   (unused_clause(_,_,_) -> print('% Removed line numbers:')
      ; print('% No clauses can be sliced away')
   ),nl,
   fail.
tcltk_print_sliced_line_numbers :-
   unused_clause(Nr,FirstLine,LastLine),
   Nr1 is Nr-1,
   print('del: '), print(FirstLine),nl,
   print(' to: '), print(LastLine),nl,
   fail.
tcltk_print_sliced_line_numbers.
  
 

extract_reverse([],B,B,E,E).
extract_reverse([(F,L)|T],B,BR,E,ER) :- extract_reverse(T,[F|B],BR,[L|E],ER).

/* ---------------- */
/* resultant_body/4 */
/* ---------------- */

pre_condition(resultant_body(Chtree,GoalID,Goal,_Body)) :-
	term_is_of_type(Chtree,chtree),
	term_is_of_type(GoalID,nodeid),
	term_is_of_type(Goal,goal).
post_condition(resultant_body(_Chtree,_GoalID,_Goal,Body)) :-
	term_is_of_type(Body,goal).

resultant_body(empty,_GoalID,_Goal,_Leaf) :- fail.
resultant_body(success,_GoalID,_Goal,[]).
resultant_body(stop,GoalID,Goal,Body) :-
	gennum(ResultantNr),
	pp_cll(more_specific_transformation(Goal)),
	pp_mnf(rename_resultant_goal(Goal,ResultantNr,GoalID,Body)).
resultant_body(select(SelLitNr,Chpaths),GoalID,Goal,Body) :-
	pp_cll(more_specific_transformation(Goal)),
	pp_mnf(split_list(Goal,SelLitNr,Left,SelCall,Right)),
	peel_off_calls(SelCall,Sel),
	member(match(ClauseNr,SubTree),Chpaths),
	(claus(ClauseNr,Sel,CBody) -> (add_used_clause(ClauseNr))
		; (print('### Error: clause not matching in resultant_body/4'),nl,
		   print('###  ClauseNr:'),print(ClauseNr),nl,
		   print('###  SelAtom: '),print(Sel),nl,
		   print('###  GoalID: '),print(GoalID),nl,
		   print('###  Goal: '),print(Goal),nl,
		   print('###  Body: '),print(Body),nl,fail)
	),
	pp_mnf(append(CBody,Right,IntGoal)),
	pp_mnf(append(Left,IntGoal,NewGoal)),
	pp_cll(resultant_body(SubTree,GoalID,NewGoal,Body)).
resultant_body(built_in_eval(NrOfBI,BI,SubTree),GoalID,Goal,Body) :-
	pp_cll(more_specific_transformation(Goal)),
	pp_mnf(split_list(Goal,NrOfBI,Left,Sel,Right)),
	(get_predicate_through_calls(Sel,BI) -> true
	 ;  (print('### Warning: illegal built-in in resultant_body/4'),nl,
	     print('###  '),print(Sel), print(' is not '),
	     print(BI),nl
	    )
	),
	((is_callable_built_in_literal(Sel))
		-> (call_built_in(Sel), Body = RBody)
		; (built_in_generates_bindings(Sel)
		   -> (print('### Error: illegal built-in in resultant_body/4'),nl,
		       print('###  '),print(Sel),
		       print(': generates bindings and is not callable'),nl,
		       Body = [Sel|RBody]
		      )
		    ;  (Body = RBody
		       )
		  )
	),
	pp_mnf(append(Left,Right,NewGoal)),
	pp_cll(resultant_body(SubTree,GoalID,NewGoal,RBody)).
resultant_body(remove(SelLitNr,_Predicate,SubTree),GoalID,Goal,Body) :-
	pp_cll(more_specific_transformation(Goal)),
	pp_mnf(split_list(Goal,SelLitNr,Left,_Sel,Right)),
	pp_mnf(append(Left,Right,NewGoal)),
	assert_removed_predicate(_Predicate),
	pp_cll(resultant_body(SubTree,GoalID,NewGoal,Body)).


/* ----------------------- */
/* rename_resultant_goal/4 */
/* ----------------------- */

pre_condition(rename_resultant_goal(Goal,ResultantNr,GoalID,_Body)) :-
	term_is_of_type(Goal,goal),
	term_is_of_type(ResultantNr,resultant_nr),
	term_is_of_type(GoalID,nodeid).
post_condition(rename_resultant_goal(_Goal,_ResultantNr,_GoalID,Body)) :-
	term_is_of_type(Body,goal).

rename_resultant_goal(UPGoal,ResultantNr,GoalID,Body) :-
	l_peel_off_calls(UPGoal,Goal),
	get_literal_numbers(Goal,1,Nrs),
	pp_mnf(rename_resultant_goal(Goal,Nrs,ResultantNr,GoalID,Body)),
	debug_print(rrg(Goal,Body)),debug_nl.


pre_condition(rename_resultant_goal(Goal,Nrs,ResultantNr,GoalID,_Body)) :-
	term_is_of_type(Goal,goal),
	term_is_of_type(Nrs,list(integer)),
	term_is_of_type(ResultantNr,resultant_nr),
	term_is_of_type(GoalID,nodeid).
post_condition(rename_resultant_goal(_Goal,_SelNr,_ResultantNr,_GoalID,Body)) :-
	term_is_of_type(Body,goal).


rename_resultant_goal([],[],_ResultantNr,_GoalID,[]).
rename_resultant_goal([Lit|T],[SelNr|TN],ResultantNr,GoalID,RenamedGoal) :-
	\+(is_constraint_literal(Lit)),
	ChPos = chpos(ResultantNr,SplitIndication),
	gt_node_descends_from(DescNodeID,GoalID,ChPos),
	literal_covered_by_split_indication(SelNr,SplitIndication),!,
	split_according_to_split_indication([Lit|T],[SelNr|TN],SplitIndication,
		InGoal,_InN,RestGoal,RestN),
	(( (SplitIndication = built_in(_NrB)) ; (SplitIndication = neg(_NrN)) )
	-> (InGoal = [Literal],
	    (cg_extract_positive_atom_from_literal(Literal,Atom,Struct,Ptr)
	     -> (pp_mnf(get_filtered_version([Atom],DescNodeID,FilteredGoal)),
		 (var(Struct)
		  -> (FilteredInGoal = FilteredGoal)
		  ;  (FilteredInGoal = [Struct],
		      make_conjunction(FilteredGoal,Ptr)
		     )
		 )
		)
	     ;  (FilteredInGoal = [Literal])
	    )
           )
	;  pp_mnf(get_filtered_version(InGoal,DescNodeID,FilteredInGoal))
	),
	pp_mnf(rename_resultant_goal(RestGoal,RestN,ResultantNr,GoalID,RT)),
	pp_mnf(append(FilteredInGoal,RT,RenamedGoal)).
rename_resultant_goal([Lit|T],[_SelNr|TN],ResultantNr,GoalID,[Lit|RT]) :-
	/* keep Lit unchanged if it occurs in no split indication */
	pp_mnf(rename_resultant_goal(T,TN,ResultantNr,GoalID,RT)).

make_conjunction([],true).
make_conjunction([Lit],Lit) :- !.
make_conjunction([Lit|T],','(Lit,CT)) :- make_conjunction(T,CT).

split_according_to_split_indication([],[],_SplitIndication,[],[],[],[]).
split_according_to_split_indication([Lit|T],[SelNr|TN],SplitIndication,
		[Lit|InGoal],[SelNr|InN],RestGoal,RestN) :-
	literal_covered_by_split_indication(SelNr,SplitIndication),!,
	split_according_to_split_indication(T,TN,SplitIndication,
		InGoal,InN,RestGoal,RestN).
split_according_to_split_indication([Lit|T],[SelNr|TN],SplitIndication,
		InGoal,InN,[Lit|RestGoal],[SelNr|RestN]) :-
	split_according_to_split_indication(T,TN,SplitIndication,
		InGoal,InN,RestGoal,RestN).

literal_covered_by_split_indication(SelNr,[H|T]) :-
	member(SelNr,[H|T]).
literal_covered_by_split_indication(SelNr,built_in(SelNr)).
literal_covered_by_split_indication(SelNr,neg(SelNr)).


cg_extract_positive_atom_from_literal(X,Atom,Ptr,Ptr) :-
	var(X),!, Atom = call(X).
/*	print('/'),print('* '),
	print('### Warning: Variable as a call in a literal !'),nl,
	print('   ### Keep Original Program for Correctness !'),nl,
	print(' *'),print('/'),nl. */
cg_extract_positive_atom_from_literal(not(X),Atom,not(Struct),Ptr) :- !,
	cg_extract_positive_atom_from_literal(X,Atom,Struct,Ptr).
cg_extract_positive_atom_from_literal(\+(X),Atom,\+(Struct),Ptr) :- !,
	cg_extract_positive_atom_from_literal(X,Atom,Struct,Ptr).
cg_extract_positive_atom_from_literal(call(X),Atom,Struct,Ptr) :- !,
	cg_extract_positive_atom_from_literal(X,Atom,Struct,Ptr).
cg_extract_positive_atom_from_literal(Atom,Atom,Ptr,Ptr) :-
	not(is_built_in_literal(Atom)).



cg_extract_positive_atom_or_builtin_from_literal(X,Atom,Ptr,Ptr) :-
	var(X),!, Atom = call(X).
/*	print('/'),print('* '),
	print('### Warning: Variable as a call in a literal !'),nl,
	print('   ### Keep Original Program for Correctness !'),nl,
	print(' *'),print('/'),nl. */
cg_extract_positive_atom_or_builtin_from_literal(not(X),Atom,not(Struct),Ptr) :-
	!,
	cg_extract_positive_atom_or_builtin_from_literal(X,Atom,Struct,Ptr).
cg_extract_positive_atom_or_builtin_from_literal(\+(X),Atom,\+(Struct),Ptr) :-
	!,
	cg_extract_positive_atom_or_builtin_from_literal(X,Atom,Struct,Ptr).
cg_extract_positive_atom_or_builtin_from_literal(call(X),Atom,Struct,Ptr) :- !,
	cg_extract_positive_atom_or_builtin_from_literal(X,Atom,Struct,Ptr).
cg_extract_positive_atom_or_builtin_from_literal(Atom,Atom,Ptr,Ptr).


/* ---------------------- */
/* get_filtered_version/3 */
/* ---------------------- */

/* Given a Goal and a NodeID return the filtered/renamed version of the goal */
/* Note: might have to traverse the instance_of and abstracted_by links in the
         global tree */

pre_condition(get_filtered_version(Goal,NodeID,_FilteredGoal)) :-
	term_is_of_type(Goal,goal),
	term_is_of_type(NodeID,nodeid).
post_condition(get_filtered_version(_Goal,_NodeID,FilteredGoal)) :-
	term_is_of_type(FilteredGoal,goal).

get_filtered_version(Goal,NodeID,[FilteredAtom]) :-
	cg_filter_goal(NodeID,G,MsvG,FG),!,
	(gt_node_chtree(NodeID,empty)
	 -> (FilteredAtom = fail)
	 ;  ((divide_constraint_goal(Goal,OrdGoal,_),
	      divide_constraint_goal(MsvG,OrdMsvG,_),
	      OrdGoal = OrdMsvG, FilteredAtom = FG)
	     -> true
	     ;  (print('### Warning: unable to filter:'),nl,
	         print('### '),print(Goal),print(' '), print(G),nl,
	         print('### '),print(OrdGoal),print(' '), print(OrdMsvG),nl,
		 FilteredAtom = filter(Goal)
		)
	    )
	).
get_filtered_version(Goal,NodeID,FilteredGoal) :-
	gt_node_instance_of(NodeID,GenID),!,
	get_filtered_version(Goal,GenID,FilteredGoal).
get_filtered_version(Goal,NodeID,FilteredGoal) :-
	gt_node_pe_status(NodeID,abstracted(_ImpStat)),!,
	pp_mnf(rename_resultant_goal(Goal,abstracted,NodeID,FilteredGoal)).

/* ---------------------- */
/* get_filtered_goal_id/2 */
/* ---------------------- */

/* Given a NodeID return the ID of the node
 which the filtered/renamed version will use */
/* Note: might have to traverse the instance_of and abstracted_by links in the
         global tree */
         
get_filtered_goal_id(NodeID,FilteredID) :-
    cg_filter_goal(NodeID,_G,_MsvG,_FG),!,FilteredID=NodeID.
get_filtered_goal_id(NodeID,FilteredID) :-
	gt_node_instance_of(NodeID,GenID),!,
	get_filtered_goal_id(GenID,FilteredID).
get_filtered_goal_id(NodeID,FilteredID) :-
	gt_node_pe_status(NodeID,abstracted(_ImpStat)),!,
	gt_node_descends_from(DescNodeID,NodeID,_ChPos),
	get_filtered_goal_id(DescNodeID,FilteredID).

/* ----------------------------------------------------------- */
/* filter_goal(Goal,NrOfTheFilteredPred,ResultingFilteredAtom) */
/* ----------------------------------------------------------- */


pre_condition(filter_goal(Goal,NrOfPredToGenerate,_FilteredAtom)) :-
	term_is_of_type(Goal,goal),
	term_is_of_type(NrOfPredToGenerate,integer).
post_condition(filter_goal(_Goal,_NrOfPredToGenerate,FilteredAtom)) :-
	term_is_of_type(FilteredAtom,nonvar).

filter_goal(Goal,NrOfPredToGenerate,FilteredAtom) :-
	divide_constraint_goal(Goal,OGoal,_CGoal),
	varlist(OGoal,Varlist),
	debug_print(filter_goal_varlist(Goal,OGoal,Varlist)),debug_nl,
	OGoal = [H|T],
	(var(H)
	 -> (FPred = call2) /* should not normally occur */
	 ;  (H =.. [FPred|_HArgs])
	),
	name(PE_Sep,"__"),
	(( (FPred = '.') ; (FPred = ':') )
	-> (string_concatenate('conj',PE_Sep,IntPred))
	;  ((T=[])
		-> string_concatenate(FPred,PE_Sep,IntPred)
		;  (string_concatenate(FPred,'_conj',FPred2),
		    string_concatenate(FPred2,PE_Sep,IntPred)
		   )
	    )
	),
	string_concatenate(IntPred,NrOfPredToGenerate,NewPred),
	FilteredAtom =.. [NewPred|Varlist].


/* ============================================ */
/*                PRINTING CLAUSES              */
/* ============================================ */



/* -------------------- */
/* print_clause_with_nl */
/* -------------------- */

print_clause_with_nl(Head,Body) :-
	numbervars(clause(Head,Body),0,_), 
		/* ^^ REMOVE THIS CALL TO PRINT VARIABLES WITH UNDERSCORES */
	filter_print_atom(Head),
	NewBody = Body,/* remove_redundant_calls(Body,NewBody,[]), */
	print_body_with_nl(NewBody),
	newlinebreak,
	fail.
print_clause_with_nl(_Head,_Body).


remove_redundant_calls([],[],_Prev).
remove_redundant_calls([H|T],NewBody,Prev) :-
	(redundant_call(H,Prev)
	-> (remove_redundant_calls(T,NewBody,Prev))
	;  (NewBody = [H|NewT],
	    remove_redundant_calls(T,NewT,[H|Prev])
	   )
	).

redundant_call(true,_Prev).
redundant_call(not(fail),_Prev).
redundant_call(\+(fail),_Prev).
redundant_call(H,Prev) :-
	ok_to_remove_duplicates(H),
	call_exists_in_list(H,Prev),!.

ok_to_remove_duplicates(Lit) :-
	is_negative_literal(Lit,_A),!.
ok_to_remove_duplicates(Lit) :-
	is_built_in_literal(Lit),!.
ok_to_remove_duplicates(_Lit) :-
	allow_removal_of_duplicate_predicates(yes).

call_exists_in_list(Call,List) :-
	member(ExCall,List),
	Call==ExCall.

/* ------------------ */
/* print_body_with_nl */
/* ------------------ */

/* print the body of a clause in the proper format */

print_body_with_nl([]) :- print('.').
print_body_with_nl([Call|T]) :-
	print(' :'),print('- '),
	nl,
	print('    '), print_call(Call),
	print_body1_with_nl(T),
	print('.').
print_body1_with_nl([]).
print_body1_with_nl([Call|T]) :-
	print(', '),nl,
	print('    '),
	print_call(Call),
	print_body1_with_nl(T).

/* ---------- */
/* print_body */
/* ---------- */

/* print the body of a clause in the proper format */

print_body([]) :- print('.').
print_body([Call|T]) :-
	print(' :'),print('- '),
	print_call(Call),
	print_body1(T),
	print('.').
print_body1([]).
print_body1([Call|T]) :-
	print(', '),print_call(Call),
	print_body1(T).

print_call(Call) :-
	(is_negative_literal(Call,NegatedAtom)
	-> (print_bold('\\+('),filter_print_atom(NegatedAtom),print_bold(')'))
	;  ((nonvar(Call),infix_predicate(Call),Call =.. [Pred,Arg1,Arg2])
	    -> (filter_print_atom(Arg1),
	        print(' '),print(Pred),print(' '),
		    filter_print_atom(Arg2))
	    ; filter_print_atom(Call)
	   )
	).

filter_print_atom('jit_merge_point') :- !, print('true /* jit_merge_point */ ').
filter_print_atom('promote_ground'(X)) :- !, print('true /* promote_ground/1 */ ').
filter_print_atom(':'(Module,Pred)) :- !,
	filter_print_atom(Module),print(':'),
	filter_print_atom(Pred).
filter_print_atom(X) :- X='$VAR'(_),!,
	print_red(X).
filter_print_atom(Atom) :-
	Atom =.. [Pred],!,
	print_faithful_functor(Pred).
filter_print_atom(Atom) :-
	Atom =.. [Pred|Args],
	Args = [_|_],!,
	print_faithful_functor(Pred),
	print('('),
	l_filter_print(Args),
	print(')').
filter_print_atom(Atom) :- print_atom(Atom).

l_filter_print([H|T]) :-
	filter_print_arg(H),
	l_filter_print1(T).

l_filter_print1([]).
l_filter_print1([H|T]) :-
	print(','),
	filter_print_arg(H),
	l_filter_print1(T).

filter_print_arg(X) :- var(X),!,
	print_red(X).
filter_print_arg(X) :- X='$VAR'(_),!,
	print_red(X).
filter_print_arg(X) :-
	print_faithful(X). /* print_green */



print_atom(Atom) :- print(Atom).
	/* or call print_faithful (but this conflicts with numbervars) !!! */

/* ---------------- */
/* print_faithful/1 */
/* ---------------- */


print_faithful(X) :- var(X),!,print_red(X).
print_faithful(X) :- X='$VAR'(_),!,
	print_red(X). /* remove  to faithfully print $VAR variables */
print_faithful(Struct) :-
	Struct =.. [_F],!,
	print_faithful_functor(Struct).
print_faithful([H|T]) :- !,
	print('['),
	print_faithful(H),
	print_faithful_list(T),print(']').
print_faithful(Struct) :-
	Struct =.. [F,Arg1|Args],
	print_faithful_functor(F),print('('),
	print_faithful(Arg1),
	l_print_faithful(Args,63),
	print(')').


l_print_faithful([],_).
l_print_faithful([H|T],Nr) :-
	print(','),
	((Nr>1, Nr\==[])
	-> (print_faithful(H), Nr1 is Nr - 1,
	    l_print_faithful(T,Nr1)
	    )
	;  (print('xtra('),
	    print_faithful(H),
	    l_print_faithful(T,63),
	    print(')')
	   )
	).


l_print_faithful([]).
l_print_faithful([H|T]) :-
	print(','),
	print_faithful(H),
	l_print_faithful(T).

print_faithful_list(X) :- var(X),!,print('|'),print(X).
print_faithful_list([]) :- !.
print_faithful_list([H|T]) :- !,
	print(','),print_faithful(H),print_faithful_list(T).
print_faithful_list(X) :- print('|'),print_faithful(X).

print_faithful_functor(F) :-
	(quote_functor(F)
	-> (print(''''),print_functor(F),print(''''))
	;  (print(F))
	).

print_functor('\\==') :- !,print('\\=='). /* ensure extra backspace printed */
print_functor('\\=') :- !,print('\\='). /* ensure extra backspace printed */
print_functor(F) :- print(F).

infix_predicate('\\=='(_X,_Y)).
infix_predicate('\\='(_X,_Y)).
infix_predicate('is'(_X,_Y)).
infix_predicate('=..'(_X,_Y)).
infix_predicate('<'(_X,_Y)).
infix_predicate('=<'(_X,_Y)).
infix_predicate('>'(_X,_Y)).
infix_predicate('>='(_X,_Y)).
infix_predicate('#='(_,_)) :- clpfd_active(yes).
infix_predicate('#<'(_,_)) :- clpfd_active(yes).
infix_predicate('#>'(_,_)) :- clpfd_active(yes).
infix_predicate('#<=>'(_,_)) :- clpfd_active(yes).
infix_predicate(in(_,_)) :- clpfd_active(yes).

quote_functor('(') :- !.
quote_functor(')') :- !.
quote_functor('[') :- !.
quote_functor(']') :- !.
quote_functor('{') :- !.
quote_functor('}') :- !.
quote_functor('~') :- !.
quote_functor('.') :- !.
quote_functor(',') :- !.
quote_functor(';') :- !.
quote_functor('+') :- !.
quote_functor('$VAR') :- !.
quote_functor('=') :- !.
quote_functor('<') :- !.
quote_functor('=<') :- !.
quote_functor('>') :- !.
quote_functor('>=') :- !.
quote_functor('=..') :- !.
quote_functor('?') :- !.
quote_functor('\\==') :- !.
quote_functor('\\=') :- !.
quote_functor([]) :- !,fail. /* don't quote nil */
quote_functor(F) :-
	atomic(F),
	name(F,Name),
	Name = [FirstChar|_],
	((FirstChar>64, FirstChar<91) /* uppercase */
	 ; (FirstChar = 95)), /* underscore */
	!.
quote_functor(F) :-
	atomic(F),
	name(F,Name),
	member(AsciiChar,Name),
	strange_ascii_for_name(AsciiChar),!.

strange_ascii_for_name(X) :-
	X<48,!.
strange_ascii_for_name(X) :-
	X>122,!.
strange_ascii_for_name(X) :-
	X>57,X<65,!.
strange_ascii_for_name(X) :-
	X>90,X<97,(X\=95),!.
