:- module(main_functions,
	[
	    unfolding_leads_to_loop/1,
	    abnormal_goal_encountered/1, error_in_pe_goal_encountered/0,
	    top_level_pegoal/1,
	    top_level_peconstraint/1,
	    abstract_partial_deduction_enabled/1,
	    reset_unfolding_leads_to_loop/0,
	    reset_abnormal_goal_encountered/0,
	    set_abnormal_goal_encountered/0,
	    pe/1, pe_without_pp/2, pe_post_process/1,
	    reset_unf_time/0,
	    add_unf_time/1,
	    flow_analysis/1,
	    add_leaves/3,
	    find_unimposed_variant/2,
	    find_unimposed_variant/3,
	    find_any_unimposed_variant/2,
	    find_any_unimposed_variant/3,
	    find_unimposed_instance/2,
	    find_unimposed_instance/3,
	    abstract_and_replace/5,
	    add_abstractions/3,
	    print_specialised_msv_program_to_file/0,
	    print_specialised_program_to_file/1,
	    print_specialised_program_to_file/0,
	    read_in_file/1,
	    read_atom/2,
	    customise/0
	]).

:- use_package( .(ecce_no_rt) ).

/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,96,97,98  */
/* --------------------------------------------- */

/* TO GET THE LATEST VERSION OF THE ECCE SYSTEM GO TO:
	http://www.cs.kuleuven.ac.be/~michael */
/* BUG REPORTS, QUESTIONS AND COMMENTS CAN BE SENT TO:
	michael@cs.kuleuven.ac.be */

/* file: ecce_main.pro */

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).


:- use_module(library(dec10_io)).
:- use_module(library(lists)).

:- include( multi_meta ).


:- use_module(dynpreds).
:- use_module(bimtools).

:- use_module(global_tree).
:- use_module(calc_chtree).
:- use_module(code_generator).
:- use_module(code_generator_isabelle).
:- use_module(dot_generator).
:- use_module(chtree_tools).
:- use_module(parametric_files).
:- use_module(msv_analysis).
:- use_module(determinate_post_unfold).
:- use_module(unfold_history).
:- use_module(benchmark).
:- use_module(post_processor).
:- use_module(homeomorphic).
:- use_module(index_tools).
:- use_module(static_dynamic_functors).
/* :- use_module('static_conjunction'). */
:- use_module(raf_analysis).
:- use_module(dead_code_elimination).
:- use_module(modes).
:- use_module(unfold_helper).
:- use_module(ic_gen).

:- use_module('postprune/post_prune').
:- use_module('abstract/abstract').
:- use_module('whistle/whistle').
:- use_module('check_instance_of/check_instance_of').


:- data unfolding_leads_to_loop/1.
unfolding_leads_to_loop(ppp).
/* can be used by selectionrules and
  post-processing pruning to register dangerous atoms to unfold,... */


:- data abnormal_goal_encountered/1.
abnormal_goal_encountered(no).

:- data top_level_pegoal/1.
:- data top_level_peconstraint/1.
top_level_pegoal(none).
top_level_peconstraint(none).


abstract_partial_deduction_enabled(no).


reset_unfolding_leads_to_loop :-
	retract_fact(unfolding_leads_to_loop(_X)),fail.
reset_unfolding_leads_to_loop.

reset_abnormal_goal_encountered :-
	retract_fact(abnormal_goal_encountered(_X)),fail.
reset_abnormal_goal_encountered :-
	assertz_fact(abnormal_goal_encountered(no)).

set_abnormal_goal_encountered :-
	retract_fact(abnormal_goal_encountered(_X)),fail.
set_abnormal_goal_encountered :-
	assertz_fact(abnormal_goal_encountered(yes)).

:- data error_in_pe_goal_encountered/0.

:- dynamic flowanalysistime/1.
:- dynamic pptime/1.

pe(Goal) :- pe_without_pp(Goal,PEGoal),pe_post_process(PEGoal).

pe_without_pp(Goal,PEGoal) :-
	(Goal = [_|_]
	 -> PEGoal = Goal, PEConstraint = []
	 ;  (Goal = (PEGoal,PEConstraint) -> true
                   ; PEGoal = [Goal], PEConstraint = [])
	),
	(term_is_of_type(PEGoal,goal,no)
	 -> true
	 ;   print('ILLEGAL GOAL: '),print(goal),nl,
	     print('Contains variables as literals (use call/1)'),nl,
	     print(' or is an open-ended list.'),nl,
	     fail
	),
	(goal_contains_undefined_literal(PEGoal)
	 ->  nl,print('###'),nl,
	     print('### Goal contains undefined calls --> will fail !'),nl,
	     print('### Be sure to read in all necessary files !'),nl,
	     print('###'),nl,nl,
	     assert(error_in_pe_goal_encountered)
	 ;  true
	),
	init_gt,
	add_gt_croot(PEGoal,PEConstraint,_ID),
	(retract_fact(top_level_pegoal(_)) -> true ; true),
	(retract_fact(top_level_peconstraint(_)) -> true ; true),
	assertz_fact(top_level_pegoal(PEGoal)),
	assertz_fact(top_level_peconstraint(PEConstraint)),
	reset_abnormal_goal_encountered,
	reset_unfolding_leads_to_loop,
	reset_static_functors,
	retractall(flowanalysistime(_)),
	retractall(ppime(_)),
	(debug_printing(on) -> (print('-> parameters: '),print_parameters,nl) ; true),
	verbose_println('-> calculating static functors'),
	calculate_static_functors,
	calculate_static_functors_for_query(PEGoal),
	/* print('-> calculating static conjunctions'),nl,go_sca(PEGoal),nl, */
	verbose_println('-> pre-processing msv phase'),
	calc_and_store_msv_result,verbose_nl,
	verbose_println('-> performing flow analysis'),
	reset_unf_time,
	time(flow_analysis(1),Time),
	assert(flowanalysistime(Time)),
	verbose_println('-> removing superfluous polyvariance'),
	time(post_processing,PostTime),
	assert(pptime(Time)),
	verbose_println('-> generating resultants'),
	generate_code.
	
pe_post_process(PEGoal) :-	
	(perform_post_msv_analysis(yes)
	 ->  clear_database,
	     copy_specialised_program_to_input,
	     verbose_println('-> msv analysis'),
	     run_msv_anlysis
	 ;  true
	),
	((perform_determinate_post_unfolding(yes),
	  generate_slice_instead_of_spec_prog(no))
	 ->  clear_database,
	     copy_specialised_program_to_input,
	     verbose_println('-> determinate post unfolding'),
	     calculate_post_unfolded_clauses
	 ;  true
	),
	(generate_slice_instead_of_spec_prog(no)
	  -> ( pp_mnf(filter_top_level_call(PEGoal,FilPEGoal)),
	       pp_mnf(append(FilPEGoal,PEGoal,GoalsOfInterest)) )
	  ;  (GoalsOfInterest = PEGoal)
	),
	
	(( /* generate_slice_instead_of_spec_prog(no), */
	  perform_dce(yes))
	 ->  clear_database,
	     copy_specialised_program_to_input,

	     verbose_print('-> dead code removal (DCE) '),
	     verbose_print(GoalsOfInterest),
         verbose_nl,
	     dead_code_elimination(GoalsOfInterest)
	 ;  true
	),
	(perform_raf(yes)
	 ->  clear_database,
	     copy_specialised_program_to_input,
	     verbose_println('-> redundant argument filtering (RAF)'),
	     perform_raf_analysis(GoalsOfInterest)
	 ;  true
	),
	(perform_far(yes)
	 ->  clear_database,
	     copy_specialised_program_to_input,
	     verbose_println('-> reverse redundant argument filtering (FAR)'),
	     perform_far_analysis(GoalsOfInterest)
	 ;  true
	),
	((output_to_file(File),File\=screen)
	->  verbose_print('Writing Specialised Program to: '),
	    verbose_print(File),verbose_nl, told, tell(File)
	;  true
	),
	nl,
	print_html('<h3>'),
	print('/'),print('* '),
	print('Specialised program generated by ECCE'),
	print_ecce_version,
	print(' *'),print('/'),newlinebreak,
	print_html('</h3>'),
	html_begin_orange,
	print('/'),print('* '),
	print('PD Goal: '), numbervars(Goal,0,_), print(Goal),
	print(' *'),print('/'),newlinebreak,
	print('/'),print('* '),
	print('Parameters: '), print_parameters,
	print(' *'),print('/'),newlinebreak,
	print('/'),print('* '),
	print('Transformation time: '), (flowanalysistime(Time) -> print(Time) ; true),
	print(' ms *'),print('/'),newlinebreak,
	print('/'),print('* '),
	print('Unfolding time: '), unf_time(UnfTime), print(UnfTime),
	print(' ms *'),print('/'),newlinebreak,
	print('/'),print('* '),
	print('Post-Processing time: '), (pptime(PostTime) -> print(PostTime) ; true),
	print(' ms *'),print('/'),html_end_color,
	newparagraph,
	(error_in_pe_goal_encountered
     -> nl,print('% ###'),nl,print('% ### Undefined call in partial evaluation goal !'),nl,print('% ###'),nl,
         newparagraph
    ; true),
	(abnormal_goal_encountered(yes)
	 -> print(':'), print('- reconsult(original_program).  /*  <---------- */'),
		newparagraph
	 ;  true
	),!,
	print_specialised_program,
	((output_to_file(File),File\=screen)
	-> told
	;  true
	).

:- data unf_time/1.
unf_time(0).

reset_unf_time :-
	retract_fact(unf_time(_OldUnfTime)),
	assertz_fact(unf_time(0)).

add_unf_time(UnfTime) :-
	retract_fact(unf_time(OldUnfTime)),
	NewUnfTime is OldUnfTime + UnfTime,
	assertz_fact(unf_time(NewUnfTime)).

flow_analysis(Count) :-
    unfold_generate_dot_file,
	(Count > 10
	 -> NewCount = 1, get_nr_of_gt_nodes_to_pe(NrOfNPE),
	    print('['),print(NrOfNPE),print(']')
	 ;  NewCount is Count + 1
	),
	pp_cll(get_gt_goal_to_pe(GoalID,Goal)),!,
	copy(Goal,G),numbervars(G,1,_),debug_println(goal_to_pe(GoalID,G)),
	(gt_node_chtree(GoalID,none)
	 -> (find_unimposed_variant(GoalID,Goal,VariantID)
	     -> (debug_println(found_variant(VariantID,Goal)),
			/* no need to unfold */
		 gt_node_chtree(VariantID,Chtree), /* just get chtree */
		 ImpStat = unimposed
		)
	     ;  (/* print(Goal),nl, */
	         debug_println(unfolding(Goal)),
		 time(pp_mnf(calculate_chtree(GoalID,Goal,Chtree)),UnfTime),
		 add_unf_time(UnfTime),
	     debug_println(unfolding_time(UnfTime)),
		 debug_print_chtree(Chtree),
		 ImpStat = unimposed
		)
	    )
	 ;  (debug_println(handling(Goal)),  /* no need to unfold */
		gt_node_chtree(GoalID,ImpChtree), /* just get imposed chtree */
		copy(Goal,CCGoal),
	        pp_mnf(remove_incorrect_builtins(ImpChtree,CCGoal,
						CorrectedChtree)),
		copy(Goal,CPPGoal),
		pp_mnf(post_prune_chtree(CPPGoal,CorrectedChtree,PChtree)),
		(PChtree = stop
		 ->  debug_println(one_step_unfolding(GoalID,Goal)),
		     one_step_unfolding(GoalID,Goal,Chtree)
		 ;   Chtree = PChtree
		),
		ImpStat = imposed
	    )
	),!, debug_nl,debug_nl,
	debug_print('Examining:'), debug_println(GoalID),
	debug_println('    '), debug_println(Goal),debug_nl,
	((Chtree=stop) -> (set_abnormal_goal_encountered) ; true),
	/* print_chtree(Chtree), */
	(pp_cll(get_instance_of(GoalID,Goal,Chtree,MoreGeneralID))
	 -> (debug_println(instance_of(MoreGeneralID)),
	     pp_mnf(mark_gt_node_as_instance_of(GoalID,MoreGeneralID)),
	     pp_mnf(mark_gt_node_as_ped(GoalID,pe(ImpStat),Chtree)),
	     verbose_print('+')
	    )
         ;  (pp_cll(whistle(GoalID,Goal,Chtree,WhistleGoalID))
	     -> (trace_print(' gen *=> '),trace_print(Goal),trace_nl,
		     mnf_call(abstract_and_replace(GoalID,Goal,Chtree,WhistleGoalID,ImpStat)))
	     ;  (trace_print(' ped  => '),trace_print(Goal),trace_nl,
		  pp_mnf(mark_gt_node_as_ped(GoalID,pe(ImpStat),Chtree)),
		  debug_println('adding leaves'),
		 mnf_call(add_leaves(GoalID,Goal,Chtree)),
		 debug_println('done adding leaves'),
		 verbose_print('.')
		)
	     )
	 ),!,
	flow_analysis(NewCount).
flow_analysis(_) :- nl,
     unfold_generate_dot_file,dot_generate_all_pdf,reset_dot_animation_counter.

add_leaves(GoalID,Goal,Chtree) :-
	get_leaf(Chtree,Goal,Leaf,ChPosition),
	(Leaf = []
	 ->  print('### add_leaves: Leaf = []'),nl
	 ;   pp_mnf(add_gt_leaf(GoalID,Leaf,ChPosition,_LeafID)),
	     verbose_println(added_leaf(GoalID,_LeafID,Leaf))
	),
	fail.
add_leaves(_,_,_).


/* ------------------------- */
/* find_unimposed_variant /2 */
/* ------------------------- */

find_unimposed_variant(Goal,VariantID) :-
	find_unimposed_variant(root,Goal,VariantID).
find_unimposed_variant(GoalID,Goal,VariantID) :-
	copy(Goal,CGoal),
	gt_node_goal(VariantID,CGoal), /* lookup a potential match */
	GoalID \= VariantID,
	gt_node_pe_status(VariantID,VPEStat),
	VPEStat \= no,
		/* VariantID should already be PE'd */
	VPEStat \= pe(imposed), VPEStat \= abstracted(imposed),
		/* otherwise chtree might be incorrect */
	gt_node_goal(VariantID,MoreGeneralGoal),
	variant_of(Goal,MoreGeneralGoal).

find_any_unimposed_variant(Goal,VariantID) :-
	find_any_unimposed_variant(root,Goal,VariantID).
find_any_unimposed_variant(GoalID,Goal,VariantID) :-
	copy(Goal,CGoal),
	gt_node_goal(VariantID,CGoal), /* lookup a potential match */
	GoalID \= VariantID,
	gt_node_pe_status(VariantID,VPEStat),
	VPEStat \= pe(imposed), VPEStat \= abstracted(imposed),
		/* otherwise chtree might be incorrect */
	gt_node_goal(VariantID,MoreGeneralGoal),
	variant_of(Goal,MoreGeneralGoal).

find_unimposed_instance(Goal,VariantID) :-
	find_unimposed_instance(root,Goal,VariantID).
find_unimposed_instance(GoalID,Goal,VariantID) :-
	copy(Goal,CGoal),
	gt_node_goal(VariantID,CGoal), /* lookup a potential match */
	GoalID \= VariantID,
	gt_node_pe_status(VariantID,VPEStat),
	VPEStat \= no,
		/* VariantID should already be PE'd */
	VPEStat \= pe(imposed), VPEStat \= abstracted(imposed),
		/* otherwise chtree might be incorrect */
	gt_node_goal(VariantID,MoreGeneralGoal),
	instance_of(Goal,MoreGeneralGoal).


/* --------------------- */
/*  ABSTRACTION SECTION  */
/* --------------------- */

abstract_and_replace(GoalID,Goal,Chtree,WhistleGoalID,_LeafImpStat) :-
	perform_parent_abstraction(yes),
	gt_node_goal(WhistleGoalID,WhistleGoal),
        debug_println(trying_abstract_parent(Goal,WhistleGoal)),
	pp_cll(abstract_parent(GoalID,Goal,Chtree,WhistleGoalID,
		NewGoals,NewChtrees)),
	((NewGoals = [split_goal(NewGoal,_SI)],NewChtrees = [_NewChtree],
	  variant_of(NewGoal,WhistleGoal)
	  /* variant_of(NewChtree,Chtree) */)
	 ->   print('### WARNING: abstract_parent performed no modification !'),nl,
	      print('### Goal = '),print(Goal),nl,
	      print('### GoalID = '), print(GoalID), print('WhistleGoalID = '),
	      print(WhistleGoalID),nl,
	      fail
	 ;   (debug_println(abstract_parent(NewGoals)),
	      !,
	      pp_mnf(remove_gt_leaves(WhistleGoalID)),
	      pp_mnf(add_abstractions(NewGoals,NewChtrees,WhistleGoalID)),
	      pp_mnf(gt_node_pe_status(WhistleGoalID,pe(WImpStat))),
	      pp_mnf(mark_gt_node_as_ped(WhistleGoalID,
					 abstracted(WImpStat),Chtree)),
	      verbose_print('*')
	     )
	).

abstract_and_replace(GoalID,Goal,Chtree,WhistleGoalID,ImpStat) :-
        verbose_println(abstracting_leaf(Goal)),
	pp_mnf(abstract_leaf(GoalID,Goal,Chtree,WhistleGoalID,NewGoals,NewChtrees)),!,
        verbose_println(abstraction_is(NewGoals)),
	((NewGoals = [split_goal(NewGoal,_SI)],NewChtrees = [NewChtree],
	  variant_of(NewGoal,Goal),
	  variant_of(NewChtree,Chtree))
	 ->  (print('### WARNING: abstract_leaf performed no modification !'),nl,
	      print('### Goal = '),print(Goal),nl,
	      print('### GoalID = '), print(GoalID), print('WhistleGoalID = '),
	      print(WhistleGoalID),nl,
	      print('### Adding Goal anyhow'),nl,
	      pp_mnf(mark_gt_node_as_ped(GoalID,pe(ImpStat),Chtree)),
	      mnf_call(add_leaves(GoalID,Goal,Chtree))
	     )
	 ;   (debug_println(abstract_leaf(NewGoals)),
	      pp_mnf(add_abstractions(NewGoals,NewChtrees,GoalID)),
	      pp_mnf(mark_gt_node_as_ped(GoalID,abstracted(ImpStat),Chtree)),
	      verbose_print('#')
	     )
	).



pre_condition(add_abstractions(NewGoals,NewChtrees,GoalID)) :-
	term_is_of_type(GoalID,nodeid),
	term_is_of_type(NewGoals,list(split_goal)),
	term_is_of_type(NewChtrees,list(ext_chtree)),
	length(NewGoals,L),
	length(NewChtrees,L).
post_condition(add_abstractions(_NewGoals,_NewChtrees,_GoalID)).

add_abstractions([],[],_GoalID).
add_abstractions([split_goal(NewGoal,SplitInd)|T],[NewChtree|CT],GoalID) :-
	(NewGoal = []
	 -> print('### add_abstractions: NewGoal = []'),nl
	 ;   pp_mnf(add_gt_leaf(GoalID,NewGoal,
				chpos(abstracted,SplitInd),NewID)),
	     pp_mnf(mark_gt_node_as_ped(NewID,no,NewChtree))
	),
	add_abstractions(T,CT,GoalID).



/* ========================================================== */
/*     PRE-  AND  POST-CONDITIONS FOR PARAMETRIC  PREDICATES  */
/* ========================================================== */

pre_condition(get_instance_of(GoalID,Goal,Chtree,_MoreGeneralID)) :-
	term_is_of_type(GoalID,nodeid),
	term_is_of_type(Goal,goal),
	term_is_of_type(Chtree,chtree).
post_condition(get_instance_of(_GoalID,_Goal,_Chtree,MoreGeneralID)) :-
	term_is_of_type(MoreGeneralID,nodeid).

pre_condition(whistle(GoalID,Goal,Chtree,_WhistleGoalID)) :-
	term_is_of_type(GoalID,nodeid),
	term_is_of_type(Goal,goal),
	term_is_of_type(Chtree,chtree).
post_condition(whistle(_GoalID,_Goal,_Chtree,WhistleGoalID)) :-
	term_is_of_type(WhistleGoalID,nodeid),
	(gt_node_pe_status(WhistleGoalID,no)
	 -> print('### WARNING: whistle returns non-partially evaluated goal'),
	     nl,fail
	 ;  true
	).


pre_condition(abstract_parent(GoalID,Goal,Chtree,WhistleGoalID,
						_NewGoals,_NewChtrees)) :-
	term_is_of_type(GoalID,nodeid),
	term_is_of_type(Goal,goal),
	term_is_of_type(Chtree,chtree),
	term_is_of_type(WhistleGoalID,nodeid).
post_condition(abstract_parent(_GoalID,_Goal,_Chtree,_WhistleGoalID,
						NewGoals,NewChtrees)) :-
	term_is_of_type(NewGoals,list(split_goal)),
	term_is_of_type(NewChtrees,list(ext_chtree)),
	length(NewGoals,L),
	length(NewChtrees,L).

pre_condition(abstract_leaf(GoalID,Goal,Chtree,WhistleGoalID,
						_NewGoals,_NewChtrees)) :-
	term_is_of_type(GoalID,nodeid),
	term_is_of_type(Goal,goal),
	term_is_of_type(Chtree,chtree),
	term_is_of_type(WhistleGoalID,nodeid).
post_condition(abstract_leaf(_GoalID,_Goal,_Chtree,_WhistleGoalID,
						NewGoals,NewChtrees)) :-
	term_is_of_type(NewGoals,list(split_goal)),
	term_is_of_type(NewChtrees,list(ext_chtree)),
	length(NewGoals,L),
	length(NewChtrees,L).


/* ========================================================== */

/* the following has been taken from 'front_end.pl' */


print_specialised_msv_program_to_file :-
	(output_to_file(File),File\=screen
	->  print('Writing Specialised Program to: '),
	    print(File),nl, told, tell(File)
	;  true
	),
	print('/'), print('* MSV Analysis Result *'), print('/'),nl,
	(msv_change
		->  print('/'), print('* New information was derived. *'), print('/')
		;   print('/'), print('* No new information ! *'), print('/')
	),nl,
	print_specialised_program,
	(output_to_file(File),File\=screen
	-> told
	;  true
	).
	
print_specialised_program_to_file(MSG) :-
	(output_to_file(File),File\=screen
	->  print('Writing Specialised Program to: '),
	    print(File),nl, told, tell(File)
	;  true
	),
	print('/'), print('*'), print(MSG), print('*'), print('/'),nl,
	print_specialised_program,
	(output_to_file(File),File\=screen
	-> told
	;  true
	).
	
print_specialised_program_to_file :-
	(output_to_file(File),File\=screen
	->  print('Writing Specialised Program to: '),
	    print(File),nl, told, tell(File)
	;  true
	),
	print_specialised_program,

	(output_to_file(File),File\=screen
	-> told
	;  true
	).

read_in_file(Filename) :-
	(on_exception(
	    existence_error(_Goal,_ArgNo,_ObjectType,_Culprit,_Reserved),
 	    see(Filename),
	    (print('*'),fail))
	-> (read_database,seen,
	    (make_iff_when_reading_clauses(on),
	     make_iff_definitions_read_in(no)
	      ->  set_make_iff_when_reading_clauses(off),
			  print('Reading in bimtools/makeiff.defs.pro'),nl,
			  see('bimtools/makeiff.defs.pro'),
			  read_database,seen,
			  set_make_iff_when_reading_clauses(on),
			  set_make_iff_definitions_read_in(yes)
	      ;   true
	    )
	   )
	;  print('### Could not open file:'),print(Filename),nl
	).

read_atom(A, T) :-
        mktemp('/tmp/readatomXXXXXX',TmpFile),
        open(TmpFile, write, TmpOut),
        display(TmpOut, A),
        display(TmpOut, ' .\n'),
        close(TmpOut),
        open(TmpFile, read, TmpIn),
        read(TmpIn, T),
        close(TmpIn).

/* ------------------------ */

customise :-
  customise_parameters. /* needs to be expanded and improved */
