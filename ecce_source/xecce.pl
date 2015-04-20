/* ====================================== */
/*  X E C C E  for Sicstus Prolog & Java  */
/* ====================================== */


/* ----------------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,96,97,98,99,2000  */
/* ----------------------------------------------------- */


/* load this file to start up the ECCE system in Sicstus Prolog
  with the Java Visual Interface (developed by Laksono Adhianto) */

%not(Goal) :- \+(Goal).

:- use_package( .('../ecce_no_rt') ).


/* --------------------- */
/* --------------------- */

:- use_module(library(lists)).
:- mode string_concatenate(i,i,o).
string_concatenate(X,Y,XY) :-
   name(X,Xs),name(Y,Ys),append(Xs,Ys,XYs),name(XY,XYs).


ensure_consulted(File) :- 
 ecce_source_directory(Dir),
 string_concatenate(Dir,File,CF),!,
 ensure_loaded(CF).

ecce_reconsult(File) :-
 ecce_source_directory(Dir),
 string_concatenate(Dir,File,CF),!,
 consult_without_redefine_warning(CF).

ecce_compile(File) :-
 ecce_source_directory(Dir),
 string_concatenate(Dir,File,CF),!,
 fcompile(CF).

ecce_get_parameters(Parameter,Descr,Val) :-
 available_parameters(Parameter,_Char,Descr,_SD,_Default),
 current_parameter_value(Parameter,Val).
 
ecce_recompile :- ecce_reconsult('ecce_compile').
 
:- ensure_consulted('bimtools/sicstus_specific.pro').

:- ensure_consulted('ecce_main.pro').


/* ------------------------------------- */
/*            Java-specific stuf
/* ------------------------------------- */

:- dynamic java_input_is_file/1.
java_input_is_file(false).

java_reload_file :-
   last_read_file(LastFileOpened),
   clear_database,
   set_make_iff_definitions_read_in(no),
   read_in_file(LastFileOpened),
   print(LastFileOpened).

/* load file to optimise */
java_open_file(Filename) :- 
   clear_database,
   set_make_iff_definitions_read_in(no),
   retract(last_read_file(_)),
   assert(last_read_file(Filename)),
   read_in_file(Filename),
   retract(java_input_is_file(_)),
   assert(java_input_is_file(true)).

/* transfer the result of optimisation to 'file to optimise' */
java_transfer_specialised_program :-
   clear_database,
   set_make_iff_definitions_read_in(no),
   copy_specialised_program_to_input,
   retract(java_input_is_file(_)),
   assert(java_input_is_file(false)).

/* perform specialisation*/
java_pe(Atom) :-
  (java_input_is_file(true) -> java_reload_file ; true),
  print('try to specialise '),print(Atom),nl,
  retract(last_pd_query(_)),
  assert(last_pd_query(Atom)),
  pe(Atom).

/* running msv analysis */
java_pe_last_query :-
  last_pd_query(Atom),
  java_pe(Atom).

java_run_msv_analysis(N) :-
 run_msv_anlysis,
 (msv_change
  -> (output_to_file(File), tell(File),
      print_specialised_program, told,
      N is 1)
  ;  (N is 0)
 ).
 
/* get the total clause stored */	
java_clause_stored(Total) :-
 next_free_clause_nr(Nr),
 Total is Nr - 1.

java_mode_nr(Mode) :-
 next_free_mode_nr(MNr),
 ((MNr > 1)
 -> (Mode is MNr - 1)
 ; (Mode is 0)).

/* redirect output to file when calling Function */
java_console(FileName, Function) :-
 print('Writing to: '), print(FileName),
 print(' call to: '), print(Function),
 nl,
 told, tell(FileName),
 call(Function),
 told.

/* post unfolding */
java_post_unfolding :-
 ((output_to_file(File),not(File=screen))
 -> (print('Writing to: '),
     print(File),nl, told, tell(File), 
     reset_spec_prog,
     print('/*Post Unfolded Program:*/'),nl,
     calc_post_unfolded_clauses,
     print_specialised_program,
     told)
 ;  (perform_determinate_post_unfolding)
 ).
java_perform_raf(Goal) :-
	print(' --> performing raf analysis'),nl,
	(Goal = [_|_]
	 -> (calculate_static_functors_for_query(Goal),
	     perform_raf_analysis(Goal))
	 ;  (calculate_static_functors_for_query([Goal]),
	     perform_raf_analysis([Goal]))
	),
	(var_call_encountered
	 -> (print('### Warning: call(_) encountered'),nl,
	     print('### Filtered program could be incorrect !'),nl,
	     print('### Keep the original program to be safe !'),nl
	    )
	 ;  true
	),
	print_specialised_program.


/* ------------------------------------------------------------*
 * ------------------------------------------------------------*/
/* get java parameters 
 * Parameter: parameter name
 * Descr: description of parameter
 * Val: parameter value
 */
java_main_parameters(Parameter, Descr, Val) :-
 available_parameters(Parameter,_Char,Descr,_SD,_Default),
 current_parameter_value(Parameter,Val).

/* get ecce main switches
 * Funct: function/predicate to call when user wants to change the value
 * Descr: switch description
 * Val: current value
 */
java_main_switches(Funct, Descr, Val) :-
 java_switch(Parameter, Descr, Funct),
 X=..[Parameter,Val],
 call(X).

java_switch(current_depth_bound,
            'Depth bound for unfolding',java_set_depth_bound).
java_switch(perform_far,
            'Reverse redundant argument Filtering (far)',java_set_far).
java_switch(perform_raf,
            'Redundant argument Filtering (raf)',java_set_raf).
java_switch(perform_dce,
            'Dead code elimination',java_set_dce).
java_switch(perform_parent_abstraction,
            'Parent abstraction',java_set_ppa).
java_switch(allow_removal_of_duplicate_predicates,
            'Removal of redundant calls',java_set_rrc).
java_switch(current_reduce_polyvariance,
            'Poly-Variance reducing post process',java_choose_reduce_polyvariance).
java_switch(perform_determinate_post_unfolding,
            'Determinate post unfolding',java_set_detpostunfold).
java_switch(perform_post_msv_analysis,
            'Bottom-up msv post propagation',java_set_postmsv).

/* --------------------------
 * list of switch predicates in form of predicate(Descr, DataType, Action)
 * Descr: switch description when user want to modify
 * DataType: 0 for Y/N, 1 for Yes/No, 2 for integer
 * Action: predicate to perform the modifaction
 * -------------------------- */
java_set_depth_bound('Enter a depth bound for unfolding (0 for no depth bound)',2,set_new_depth_bound).
java_set_far('Perform Reverse Redundant Argument Filtering (FAR)',1,set_perform_far).
java_set_raf('Perform Redundant Argument Filtering',1,set_perform_raf).
java_set_dce('Perform Dead Code Elimination',1,set_perform_dce).
java_set_ppa('Perform Parent Abstraction',1,set_perform_ppe).
java_set_rrc('Remove Redundant Calls/Clauses',1,set_perform_rrc).
java_choose_reduce_polyvariance('Minimise polyvariance',0,set_current_reduce_polyvariance).
java_set_detpostunfold('Perform Determinate Post-Unfolding',1,set_perform_determinate_post_unfolding).
java_set_postmsv('Bottom-Up Post MSV Analysis',1,set_perform_post_msv_analysis).

/* database of ecce standard configurations 
 * syntax: java_standard_config(Type, Descr, CharIdent)
 * Type: type of configuration
 * Descr: configuration descriptiuon
 * CharIdent: 
 */

java_standard_config(standard_config,
            'Best Standard PD',98).
java_standard_config(standard_config,
            'Fast but good Standard PD',102). 
java_standard_config(conjunctive,
            'Fast Determinate Conjunctive PD',100).
java_standard_config(conjunctive,
            'Conjunctive PD for Deforestation (may worsen code)',103).
java_standard_config(partial_config,
            'add miXtus like unfolding',120).
java_standard_config(partial_config,
            'Enable Abstract partial deduction',97).
java_standard_config(expert_config,
            'Ecological PD (requires depth bound)',101).
java_standard_config(expert_config,
            'Homeomorphic Ecological PD',104).
java_standard_config(expert_config,
            'Msg abstraction + homeo. on atoms',109).
java_standard_config(expert_config,
            'Simple PD (msg+not more general)',115).
java_standard_config(expert_config,
            'Karp and Miller Coverability Mode',121).
java_standard_config(expert_config,
            'Finkel Min. Coverability Mode',122).
java_standard_config(expert_config,
            'CLP(FD) Specialisation',107).
java_standard_config(expert_config,
            'Disable Post-processing',110).
java_standard_config(expert_config,
            'Re-enable Post-processing',111).
java_standard_config(expert_conjunctive,'Conjunctive PD',99).
java_standard_config(expert_conjunctive,'(C  -th- )  Whistle',105).
java_standard_config(expert_conjunctive,'(C  -hh- )  Whistle',106).

:- set_user_expert(yes).

