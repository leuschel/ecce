:- module(parametric_files,
	[
	    initialise_parameters/0,
	    customise_parameters/0, /* new */
	    print_parameters/0,
	    set_parameters/0, set_parameters_from_char/1,
	    set_switch/1,
	    choose_parameter/1,
	    set_parameter/2,
	    print_available_options/1,
	    print_available_parameters/0,
	    current_reduce_polyvariance/1,
	    choose_reduce_polyvariance/0,
	    set_current_reduce_polyvariance/1,
	    update_current_reduce_polyvariance/1,
	    set_detpostunfold/0,
	    set_postmsv/0,
	    set_raf/0,
	    set_far/0,
	    set_dce/0,
	    set_ppa/0, set_perform_ppa/1,
	    set_rrc/0,
	    choose_standard_config/0,
	    set_standard_config/1, 
	    set_perform_raf/1, 
	    set_perform_far/1,
	    set_perform_dce/1,
	    set_perform_rrc/1,
	    set_perform_determinate_post_unfolding/1,
	    set_perform_post_msv_analysis/1,
	    set_reduce_polyvariance/1,
	    set_perform_slice/1
	]).

/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,96,97,98  */
/* --------------------------------------------- */

/* TO GET THE LATEST VERSION OF THE ECCE SYSTEM GO TO:
	http://www.cs.kuleuven.ac.be/~michael */
/* BUG REPORTS, QUESTIONS AND COMMENTS CAN BE SENT TO:
	michael@cs.kuleuven.ac.be */


:- use_package( .('ecce_no_rt') ).


:- use_module(dynpreds).

/* file: parameteric_files.pro */

:- use_module(bimtools).

:- multifile available_options/4.
:- multifile available_ciaopp_option/3.

:- use_module(parameters).
:- use_module(depth_bound).

:- use_module('abstract/abstract').
:- use_module('check_instance_of/check_instance_of').
:- use_module('more_specific/more_specific').
:- use_module('neg_solve/neg_solve').
:- use_module('partition/partition').
:- use_module('selectionrule/selectionrule').
:- use_module('postprune/post_prune').
:- use_module('whistle/whistle').

%:- use_module('abstractpd/calc_chtree_pd').
%:- use_module('abstractpd/flow_analysis').
%:- use_module('constraints/constraints.clpfd').

/* predicates potentially re-loaded from file: */


:- set_prolog_flag(multi_arity_warnings,off).
%:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).


/* parametric files: */

initialise_parameters :-
	available_parameters(Parameter,_Char,_Descr,_SD,Default),
	format('Initialising Parameter ~w = ~w~n',[Parameter,Default]),
	set_parameter(Parameter,Default),
	fail.
initialise_parameters.


print_parameters :-
	available_parameters(Parameter,_Char,_Descr,SD,_Default),
	print(SD),
	print(':'),
	current_parameter_value(Parameter,Val),
	ecce_put(Val),
	print(' '),fail.
print_parameters :-
	print('Raf:'),perform_raf(Raf),print(Raf),
	print('Far:'),perform_far(Far),print(Far),
	print(' Dce:'),perform_dce(Dce),print(Dce),
	print(' Poly:'),current_reduce_polyvariance(V),ecce_put(V),
	print(' Dpu:'),perform_determinate_post_unfolding(DPU),print(DPU),
	print(' ParAbs:'),perform_parent_abstraction(PPE),print(PPE),
	print(' Msvp:'),perform_post_msv_analysis(PSMV),print(PSMV),
	print(' Rrc:'),allow_removal_of_duplicate_predicates(RRC),print(RRC).



/* ===================== */
/* SETTING THE PARAMTERS */
/* ===================== */

set_parameters :-
	print(' ---------- '),nl,
	print(' Parameters:'),nl,
	print(' ---------- '),nl,
	print_available_parameters,
	print(' --------- '),nl,
	print(' Switches:'),nl,
	print(' --------- '),nl,
	print('  d: Depth bound for unfolding  -> '),
		current_depth_bound(D),print(D),nl,
	print('  e: reverse redundant argument Filtering (far) -> '),
		perform_far(Far),print(Far),nl,
	print('  f: redundant argument Filtering (raf) -> '),
		perform_raf(Raf),print(Raf),nl,
	print('  g: dead code elimination (dce) -> '),
		perform_dce(Dce),print(Dce),nl,
	print('  h: perform parent abstraction -> '),
		perform_parent_abstraction(PPE),print(PPE),nl,
	print('  i: allow removal of redundant/duplicate calls -> '),
		allow_removal_of_duplicate_predicates(RRC),print(RRC),nl,
	print('  u: detect dead literals and non-left executable builtins while Unfolding -> '),
		detect_dead_literals_or_non_leftmost_builtins(UV),print(UV),nl,
	print('  v: perform poly-Variance reducing post process -> '),
		current_reduce_polyvariance(V),ecce_put(V),nl,
	print('  x: perform determinate post unfolding -> '),
		perform_determinate_post_unfolding(DPU),print(DPU),nl,
	print('  y: perform bottom-up msv post propagation -> '),
		perform_post_msv_analysis(PSMV),print(PSMV),nl,
	print('  b: compute slice instead of specialised program -> '),
		generate_slice_instead_of_spec_prog(Slice),print(Slice),nl,
	print(' ---------- '),nl,
	print('  z: choose a standard configuration'),nl,
	print('=> '),
	ecce_get(AsciiChar),
	set_parameters_from_char(AsciiChar),!.
set_parameters :-
	print('Illegal Command. Keeping Old Values.'),nl.

set_parameters_from_char(AsciiChar) :-
   (set_switch(AsciiChar)
	 -> true
	 ;  (available_parameters(Parameter,AsciiChar,_Descr,_SD,_Default),
	     choose_parameter(Parameter)
	    )
	).
set_parameters_from_char(AsciiChar) :-
	print('Illegal Parameter:'), print(AsciiChar), print('. Keeping Old Values.'),nl.

set_switch(100) :- !,set_depth_bound.
set_switch(101) :- !,set_far.
set_switch(102) :- !,set_raf.
set_switch(103) :- !,set_dce.
set_switch(104) :- !,set_ppa.
set_switch(105) :- !,set_rrc.
set_switch(98) :- !,set_slice.
set_switch(117) :- !,set_detect_dead_literals_or_non_leftmost_builtins.
set_switch(118) :- !,choose_reduce_polyvariance.
set_switch(120) :- !,set_detpostunfold.
set_switch(121) :- !,set_postmsv.
set_switch(122) :- !,choose_standard_config.


/* ---------------------- */
/* customise_parameters/0 */
/* ---------------------- */

/* parametric_files:customise_parameters. */

customise_parameters :-
  customise(P),
  fail.
customise_parameters :- nl.

customise(Parameter) :-
  available_parameters(Parameter,_Char,ParaDescr,_ShortDescr,_Default),
  print(ParaDescr), print(':'),
  findall(Descr,available_ciaopp_option(Parameter,_,Descr),DL),
  print_customise_list(DL),
  get_current_parameter_value(Parameter,Val),
  (available_ciaopp_option(Parameter,Val,CurDescr) -> true ; CurDescr='?'),
  print(' ('),print(CurDescr),print(')'),nl.


print_customise_list(L) :- print('['),print_customise_list2(L),print(']').
print_customise_list2([]).
print_customise_list2([X]) :- print(X),!.
print_customise_list2([H|T]) :- print(H),print(','),print_customise_list2(T).
  
	
/* ------------------ */
/* choose_parameter/1 */
/* ------------------ */

choose_parameter(Parameter) :-
	print('Availabe options for: '),
	available_parameters(Parameter,_Char,Descr,_ShortDescr,_Default),
	print(Descr),nl,
	print_available_options(Parameter),
	(get_current_parameter_value(Parameter,X)
	 -> (print('Current Choice -> '), ecce_put(X),nl)
	 ;  (print('No current value !'),nl)
	),
	print('=> '),
	ecce_get(AsciiChar),
	set_parameter(Parameter,AsciiChar).
	



/* ------------------ */
/*   set_parameter/2  */
/* ------------------ */

set_parameter(Parameter,Char) :-
	available_options(Parameter,Char,File,_Descr),!,
	%ecce_reconsult(File),
	update_parameter(Parameter,Char).
set_parameter(_Parameter,_Char) :-
	print('Illegal Parameter Value. Keeping Old Values.'),nl,
	print(set_parameter(_Parameter,_Char)),nl.


/* ---------------------------- */
/*   print_available_options/1  */
/* ---------------------------- */

print_available_options(Parameter) :-
	available_options(Parameter,Char,_File,Descr),
	print('  '),ecce_put(Char),print(': '),
	print(Descr),nl,fail.
print_available_options(_).


/* ------------------------------- */
/*   print_available_parameters/0  */
/* ------------------------------- */

print_available_parameters :-
	available_parameters(Parameter,Char,Descr,_SD,_Default),
	print('  '),ecce_put(Char),print(': '),
	print(Descr),
	print(' --> '),
	current_parameter_value(Parameter,Val),
	ecce_put(Val),
	nl,fail.
print_available_parameters.



/* ======================== */
/*    REDUCE POLYVARIANCE   */
/* ======================== */

:- data current_reduce_polyvariance/1.
current_reduce_polyvariance(121).

choose_reduce_polyvariance :-
	print(' Minimise polyvariance (without precision loss) in a'),nl,
	print('  post-processing phase ?'),nl,
	print('  n: No'),nl,
	print('  y: Yes'),nl,
	current_reduce_polyvariance(X),
	print('Current Choice -> '), ecce_put(X),nl,
	print('=> '),
	ecce_get(AsciiChar),
	set_current_reduce_polyvariance(AsciiChar).

set_current_reduce_polyvariance(110) :- !,
	update_current_reduce_polyvariance(110).
set_current_reduce_polyvariance(121) :- !,
	update_current_reduce_polyvariance(121).
set_current_reduce_polyvariance(_X) :-
	print('Illegal Reduce Polyvariance. Keeping Old Values.'),nl.

update_current_reduce_polyvariance(_X) :-
	retract_fact(current_reduce_polyvariance(_C)),fail.
update_current_reduce_polyvariance(X) :-
	assertz_fact(current_reduce_polyvariance(X)).

set_reduce_polyvariance(yes) :- update_current_reduce_polyvariance(121).
set_reduce_polyvariance(no) :- update_current_reduce_polyvariance(110).

/* ------------------- */
/* set_detpostunfold/0 */
/* ------------------- */

set_detpostunfold :-
	print('Perform Determinate Post-Unfolding:'),nl,
	print('yes/no'),nl,
	print('Current choice: '),
	perform_determinate_post_unfolding(Cur),
	print(Cur),nl,
	print('choice =>'),
	read(NewValue),
	((NewValue\=yes,NewValue\=no)
	 -> print('Illegal value'),nl
	 ;  (set_perform_determinate_post_unfolding(NewValue))
	).

set_perform_determinate_post_unfolding(_NewVal) :- \+(_NewVal=yes), \+(_NewVal=no),!,fail.
set_perform_determinate_post_unfolding(_NewVal) :-
	retract_fact(perform_determinate_post_unfolding(_Cur)),
	fail.
set_perform_determinate_post_unfolding(NewVal) :-
	asserta_fact(perform_determinate_post_unfolding(NewVal)).

/* ------------------- */
/* set_postmsv/0 */
/* ------------------- */

set_postmsv :-
	print('Perform Bottom-Up Post MSV Analysis:'),nl,
	print('yes/no'),nl,
	print('Current choice: '),
	perform_post_msv_analysis(Cur),
	print(Cur),nl,
	print('choice =>'),
	read(NewValue),
	((NewValue\=yes,NewValue\=no)
	 -> print('Illegal value'),nl
	 ;  (set_perform_post_msv_analysis(NewValue))
	).

set_perform_post_msv_analysis(_NewVal) :- \+(_NewVal=yes), \+(_NewVal=no),!,fail.
set_perform_post_msv_analysis(_NewVal) :-
	retract_fact(perform_post_msv_analysis(_Cur)),
	fail.
set_perform_post_msv_analysis(NewVal) :-
	asserta_fact(perform_post_msv_analysis(NewVal)).

/* --------- */
/* set_raf/0 */
/* --------- */

set_raf :-
	print('Perform Redundant Argument Filtering:'),nl,
	print('yes/no'),nl,
	print('Current choice: '),
	perform_raf(Cur),
	print(Cur),nl,
	print('choice =>'),
	read(NewValue),
	((NewValue\=yes,NewValue\=no)
	 -> print('Illegal value'),nl
	 ;  (set_perform_raf(NewValue))
	).

set_perform_raf(_NewVal) :- \+(_NewVal=yes), \+(_NewVal=no),!,fail.
set_perform_raf(_NewVal) :-
	retract_fact(perform_raf(_Cur)),
	fail.
set_perform_raf(NewVal) :-
	asserta_fact(perform_raf(NewVal)).

/* --------- */
/* set_far/0 */
/* --------- */

set_far :-
	print('Perform Reverse Redundant Argument Filtering (FAR):'),nl,
	print('yes/no'),nl,
	print('Current choice: '),
	perform_far(Cur),
	print(Cur),nl,
	print('choice =>'),
	read(NewValue),
	((NewValue\=yes,NewValue\=no)
	 -> print('Illegal value'),nl
	 ;  (set_perform_far(NewValue))
	).

set_perform_far(_NewVal) :- \+(_NewVal=yes), \+(_NewVal=no),!,fail.
set_perform_far(_NewVal) :-
	retract_fact(perform_far(_Cur)),
	fail.
set_perform_far(NewVal) :-
	asserta_fact(perform_far(NewVal)).

/* --------- */
/* set_dce/0 */
/* --------- */

set_dce :-
	print('Perform Dead Code Elimination:'),nl,
	print('yes/no'),nl,
	print('Current choice: '),
	perform_dce(Cur),
	print(Cur),nl,
	print('choice =>'),
	read(NewValue),
	((NewValue\=yes,NewValue\=no)
	 -> print('Illegal value'),nl
	 ;  (set_perform_dce(NewValue))
	).

set_perform_dce(_NewVal) :- \+(_NewVal=yes), \+(_NewVal=no),!,fail.
set_perform_dce(_NewVal) :-
	retract_fact(perform_dce(_Cur)),
	fail.
set_perform_dce(NewVal) :-
	asserta_fact(perform_dce(NewVal)).

/* --------- */
/* set_ppa/0 */
/* --------- */

set_ppa :-
	print('Perform Parent Abstraction:'),nl,
	print('yes/no'),nl,
	print('Current choice: '),
	perform_parent_abstraction(Cur),
	print(Cur),nl,
	print('choice =>'),
	read(NewValue),
	((NewValue\=yes,NewValue\=no)
	 -> print('Illegal value'),nl
	 ;  (set_perform_ppa(NewValue))
	).

set_perform_ppa(_NewVal) :- \+(_NewVal=yes), \+(_NewVal=no),!,fail.
set_perform_ppa(_NewVal) :-
	retract_fact(perform_parent_abstraction(_Cur)),
	fail.
set_perform_ppa(NewVal) :-
	asserta_fact(perform_parent_abstraction(NewVal)).


/* --------- */
/* set_rrc/0 */
/* --------- */

set_rrc :-
	print('Allow Removal of Useless Clauses and Redundant Calls:'),nl,
	print('yes/no'),nl,
	print('Current choice: '),
	allow_removal_of_duplicate_predicates(Cur),
	print(Cur),nl,
	print('choice =>'),
	read(NewValue),
	((NewValue\=yes,NewValue\=no)
	 -> print('Illegal value'),nl
	 ;  (set_perform_rrc(NewValue))
	).

set_perform_rrc(_NewVal) :-
	retract_fact(allow_removal_of_duplicate_predicates(_Cur)),
	fail.
set_perform_rrc(NewVal) :-
	asserta_fact(allow_removal_of_duplicate_predicates(NewVal)).

/* --------- */
/* set_slice/0 */
/* --------- */

set_slice :-
	print('Compute Slice instead of producing specialised code:'),nl,
	print('yes/no'),nl,
	print('Current choice: '),
	generate_slice_instead_of_spec_prog(Cur),
	print(Cur),nl,
	print('choice =>'),
	read(NewValue),
	((NewValue\=yes,NewValue\=no)
	 -> print('Illegal value'),nl
	 ;  (set_perform_slice(NewValue))
	).

set_perform_slice(_NewVal) :-
	retract(generate_slice_instead_of_spec_prog(_Cur)),
	fail.
set_perform_slice(NewVal) :-
	asserta(generate_slice_instead_of_spec_prog(NewVal)).
/* --------- */
/* set_detect_dead_literals_or_non_leftmost_builtins/0 */
/* --------- */

set_detect_dead_literals_or_non_leftmost_builtins :-
	print('Allow Detection of dead literals while unfolding:'),nl,
	print('yes/no'),nl,
	print('Current choice: '),
	detect_dead_literals_or_non_leftmost_builtins(Cur),
	print(Cur),nl,
	print('choice =>'),
	read(NewValue),
	((NewValue\=yes,NewValue\=no)
	 -> print('Illegal value'),nl
	 ;  (set_detect_dead_literals_or_non_leftmost_builtins(NewValue))
	).

set_detect_dead_literals_or_non_leftmost_builtins(NewVal) :-
	retractall(detect_dead_literals_or_non_leftmost_builtins(_Cur)),
	asserta(detect_dead_literals_or_non_leftmost_builtins(NewVal)).

/* ================ */
/*    STD CONFIGS   */
/* ================ */

choose_standard_config :-
	print(' Available Configurations:'),nl,
	print(' -----------'),nl,
	print(' Standard PD'),nl,
	print(' -----------'),nl,
	print('  b: Best standard PD (homeomorphic ecological PD + miXtus unfolding/pruning)'),nl,
	expert_print('  e: Ecological partial deduction (requires depth bound for termination)'),expert_nl,
	print('  f: Fast but good standard PD (homeomorphic ecological p.d. + o unfolding + no depth bound)'),nl,
	expert_print('  h: Homeomorphic ecological partial deduction'),
	expert_nl,
	expert_print('  m: Msg abstraction + homeomorphic emb. on atoms p.d.'),
	expert_nl,
	expert_print('  s: Simple partial deduction (msg + not more general)'),
	expert_nl,
	expert_print('  y: Karp and Miller Coverability Mode for Petri Nets'),nl,
	expert_print('  z: Finkel Min. Coverability Mode for Petri Nets'),
	expert_nl,
	print(' --------------'),nl,
	print(' Conjunctive PD'),nl,
	print(' --------------'),nl,
	expert_print('  c: Conjunctive PD'),expert_nl,
	print('  d: (Csc-th-t) fast Determinate conjunctive PD'),nl,
	expert_print('   i:(C  -th- )  Whistle'),expert_nl,
	expert_print('   j:(C  -hh- )  Whistle'),expert_nl,
	print('  g: conjunctive PD for deforestation (may worsen some programs)'),nl,
	print(' ----------------------'),nl,
	print(' Partial Configurations'),nl,
	print(' ----------------------'),nl,
	%print('  k: enable Constrained CLP(FD) Partial Deduction'),nl,
	%print('  r: enable Constrained RUL Partial Deduction'),nl,
	print('  t: for Termination Analysis: preserve non-termination'),nl,
	print('  a: enable Abstract Partial Deduction'),nl,
	print('  x: add miXtus like unfolding to current configuration'),nl,
	print('  n: disable post-processing'),nl,
	print('  o: enable full post-processing'),nl,
	print('  p: minimal standard PD'),nl,
	print('=> '),
	ecce_get(AsciiChar),
	set_standard_config(AsciiChar).

set_standard_config(97) :- !, /* Abstract PD */
        % ecce_reconsult('abstractpd/calc_chtree'),
	% ecce_reconsult('abstractpd/flow_analysis'),
        print('Abstract partial deduction enabled !'),nl,
        print(' - constrained goal syntax: ([p(X),q(X)],[ecce_type(list(any),X)])'),
        nl,
        print(' - WARNING: implementation not yet fully functional'),
        nl.
set_standard_config(98) :- !, /* best */
	set_parameter(abstraction,99), /* (100) ?? */
	set_parameter(instchecks,118),
	set_parameter(partition,115),
	set_new_depth_bound(0),
	set_parameter(msv,115),
	set_parameter(negsolve,103),
	set_parameter(selectionrule,120),
	set_parameter(postprune,120),
	set_current_reduce_polyvariance(121),
	set_parameter(whistle,104),
	set_perform_determinate_post_unfolding(yes),
	set_perform_post_msv_analysis(no),
	set_perform_raf(yes),
	set_perform_dce(yes),
	set_detect_dead_literals_or_non_leftmost_builtins(yes).
set_standard_config(default) :- !, /* the default setting: conjunctive */
	set_parameter(abstraction,108),
	set_parameter(instchecks,118),
	set_parameter(msv,115),
	set_parameter(negsolve,103),
	set_parameter(partition,101),
	set_parameter(postprune,110),
	set_parameter(selectionrule,116),
	set_parameter(whistle,102),
	set_standard_config(default_postprocess),
	set_detect_dead_literals_or_non_leftmost_builtins(yes).
set_standard_config(99) :- !, /* conjunctive */
	set_parameter(abstraction,108),
	set_parameter(instchecks,97),
	set_parameter(partition,101),
	set_parameter(postprune,99),
	set_parameter(selectionrule,108),
	set_parameter(whistle,103),
	set_detect_dead_literals_or_non_leftmost_builtins(yes).
set_standard_config(100) :- !, /* conjunctive - fast*/
	set_parameter(abstraction,108),
	set_parameter(instchecks,97),
	set_parameter(partition,101),
	set_parameter(postprune,110),
	set_parameter(selectionrule,116),
	set_parameter(whistle,102),
	set_detect_dead_literals_or_non_leftmost_builtins(yes).
set_standard_config(105) :- !, /* i: th - conj. whistle*/
	set_parameter(abstraction,108),
	set_parameter(whistle,102).
set_standard_config(106) :- !, /* j: hh - conj. whistle*/
	set_parameter(abstraction,105),
	set_parameter(whistle,100).
set_standard_config(103) :- !, /* g - deforestation */
	set_parameter(abstraction,106),
	set_parameter(instchecks,97),
	set_parameter(partition,99),
	set_parameter(postprune,110),
	set_parameter(selectionrule,104),
	set_detect_dead_literals_or_non_leftmost_builtins(yes),
	set_parameter(whistle,102).  /* changed from 100 !!!! mal 17 July 2003 */
set_standard_config(101) :- !,
	set_parameter(abstraction,101),
	set_parameter(instchecks,118),
	set_parameter(whistle,101).
set_standard_config(102) :- !, /* f - classic fast*/
	set_parameter(abstraction,99),
	set_parameter(instchecks,118),
	set_parameter(partition,115),
	set_new_depth_bound(0),
	set_parameter(msv,115),
	set_parameter(negsolve,103),
	set_parameter(selectionrule,111),
	set_parameter(postprune,110),
	set_current_reduce_polyvariance(121),
	set_parameter(whistle,104),
	set_perform_determinate_post_unfolding(yes),
	set_perform_post_msv_analysis(no),
	set_perform_raf(yes),
	set_perform_dce(yes),
	set_detect_dead_literals_or_non_leftmost_builtins(yes).
set_standard_config(104) :- !,
	set_parameter(abstraction,99),
	set_parameter(instchecks,118),
	set_parameter(whistle,104).
set_standard_config(109) :- !,
	set_parameter(abstraction,109),
	set_parameter(instchecks,118),
	set_parameter(whistle,97).
set_standard_config(115) :- !,
	set_parameter(abstraction,109),
	set_parameter(instchecks,118),
	set_parameter(whistle,109).
set_standard_config(120) :- !, /* x: Mixtus like unfolding */
	set_parameter(selectionrule,120),
	set_parameter(postprune,120).
set_standard_config(88) :- !, /* X: Mixtus */
    set_standard_config(98), set_standard_config(120).

set_standard_config(121) :- !, /* Karp&Miller */
	set_parameter(abstraction,110),
	set_parameter(instchecks,121),
	set_new_depth_bound(0),
	set_parameter(msv,110),
	set_parameter(negsolve,103),
	set_parameter(selectionrule,99),
	set_parameter(postprune,110),
	set_parameter(partition,110),
	set_parameter(whistle,111),
	set_perform_determinate_post_unfolding(no),
	set_perform_post_msv_analysis(no),
	set_perform_raf(no),
	set_perform_far(no),
	set_perform_dce(no),
	set_perform_ppa(no).

set_standard_config(122) :- !, /* Finkel */
	set_parameter(abstraction,110),
	set_parameter(instchecks,97), /* change over KarpMiller */
	set_new_depth_bound(0),
	set_parameter(msv,110),
	set_parameter(negsolve,103),
	set_parameter(selectionrule,99),
	set_parameter(postprune,110),
	set_parameter(partition,110),
	set_parameter(whistle,110), /* change over KarpMiller */
	set_perform_determinate_post_unfolding(no),
	set_perform_post_msv_analysis(no),
	set_perform_raf(no),
	set_perform_far(no),
	set_perform_dce(no),
	set_perform_ppa(yes). /* change over KarpMiller */

set_standard_config(107) :- !, /*k: CLP(FD) */
	% ensure_consulted('constraints/constraints.clpfd.pl'),
        set_parameter(instchecks,99),
	set_parameter(msv,118),
	set_parameter(partition,120),
	set_parameter(whistle,98),
	set_parameter(abstraction,111).

set_standard_config(114) :- !, /*r: RUL constrained PD */
	retractall_fact(rul_active(_)),
	assertz_fact(rul_active(yes)),
	set_parameter(instchecks,100),
	set_parameter(msv,119),
	set_parameter(partition,121),
	set_parameter(whistle,102),
	set_parameter(abstraction,113).
set_standard_config(termd) :-
    set_standard_config(term),
    set_parameter(selectionrule,114).
set_standard_config(term) :-
    set_standard_config(98), /* best std PD */
    set_standard_config(116).
set_standard_config(116) :- !, /*t: for termination analysis */
	set_parameter(selectionrule,106),
	set_perform_determinate_post_unfolding(no),
	set_perform_post_msv_analysis(no),
	set_parameter(msv,110),
	set_parameter(negsolve,103),
	set_parameter(postprune,110),
	set_perform_rrc(no),
	set_detect_dead_literals_or_non_leftmost_builtins(no).

set_standard_config(110) :- !, /* n: set minimal post-processing */
	set_current_reduce_polyvariance(110),
	set_perform_determinate_post_unfolding(no),
	set_perform_post_msv_analysis(no),
	set_perform_raf(no),
	set_perform_far(no),
	set_perform_dce(no),
	set_perform_rrc(no).

set_standard_config(111) :- !, /* o: set maximal post-processing */
	set_current_reduce_polyvariance(121),
	set_perform_determinate_post_unfolding(yes),
	set_perform_post_msv_analysis(yes),
	set_perform_raf(yes),
	set_perform_far(yes),
	set_perform_dce(yes),
	set_perform_rrc(yes).
set_standard_config(default_postprocess) :- !, 
	set_current_reduce_polyvariance(121),
	set_perform_determinate_post_unfolding(yes),
	set_perform_post_msv_analysis(no),
	set_perform_raf(yes),
	set_perform_far(yes),
	set_perform_dce(yes),
	set_perform_rrc(yes).
	
set_standard_config(112) :- !, /* p: minimal PD */
	set_parameter(abstraction,109),
	set_parameter(instchecks,97),
	set_parameter(partition,115),
	set_new_depth_bound(0),
	set_parameter(msv,110),
	set_parameter(negsolve,110),
	set_parameter(selectionrule,115),
	set_parameter(postprune,110),
	set_parameter(whistle,109).
	
	

set_standard_config(_X) :-
	print('Illegal Standard Config. Keeping Old Values.'),nl.


