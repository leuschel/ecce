:- module(dynpreds,
	[
	    print_ecce_version/0,
	    print_ecce_release/0,
	    user_expert/1,
	    set_user_expert/1,
	    expert_print/1,
	    expert_nl/0,
	    beginner_print/1,
	    beginner_nl/0,
	    toggle_user_expert/0,	    
	    exec_mode/1,
	    set_exec_mode/1,
	    toggle_exec_mode/0,	    
	    make_iff_definitions_read_in/1, 
	    set_make_iff_definitions_read_in/1,	
	    output_to_file/1,
	    set_output_to_file_int/1,
	    set_output_to_file/1,		   
	    clpfd_active/1,
	    rul_active/1,
	    constraints_active/0,		   
	    current_parameter_value/2,	    get_current_parameter_value/2,
	    perform_determinate_post_unfolding/1,
	    perform_post_msv_analysis/1,perform_raf/1,perform_far/1,perform_dce/1,
	    perform_parent_abstraction/1,
	    allow_removal_of_duplicate_predicates/1,
	    update_parameter/2,	    
	    dont_erase/3,
	    erasure_changed/0,
	    var_call_encountered/0,		   
	    reset_dont_erase/0,
	    reset_erasure_changed/0,
	    reset_var_call_encountered/0,		   
	    nr_of_erased_arguments/1,
	    reset_nr_of_erased_arguments/0,
	    inc_nr_of_erased_arguments/0,		   
	    reachable/2,
	    reachable_changed/0,
	    reset_reachable/0,
	    reset_reachable_changed/0,	    
	    generate_dot_nodes_for_leaves/1,
	    set_generate_dot_nodes_for_leaves/1,
	    generate_slice_instead_of_spec_prog/1	,
        gen_anonymous_vars_for_erased_args/1,
        detect_dead_literals_or_non_leftmost_builtins/1
	]).

:- set_prolog_flag(multi_arity_warnings,off).
%:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

print_ecce_version :- print(' '),print('2.0').
print_ecce_release :- print('Release 2, July 2010').

/* file: dynpreds.pl */

:- use_module(bimtools).

:- data user_expert/1.
user_expert(no).

set_user_expert(_Val) :-
	retract_fact(user_expert(_X)),fail.
set_user_expert(Val) :-
	assertz_fact(user_expert(Val)).

expert_print(Txt) :-
	(user_expert(yes) -> print(Txt) ; true).
expert_nl :-
	(user_expert(yes) -> nl ; true).

beginner_print(Txt) :-
	(user_expert(no) -> print(Txt) ; true).
beginner_nl :-
	(user_expert(no) -> nl ; true).

toggle_user_expert :-
	(user_expert(yes) ->
	   (print('Switching EXPERT level OFF'),nl,
	    set_user_expert(no))
	 ; (print('Switching EXPERT level ON'),nl,
	    set_user_expert(yes))
	).
	

:- data exec_mode/1.

set_exec_mode(_Val) :-
	retract_fact(exec_mode(_X)),fail.
set_exec_mode(Val) :-
	assertz_fact(exec_mode(Val)).

toggle_exec_mode :-
	(exec_mode(interactive) ->
	   (print('Switching to Command-line mode'),nl,
	    set_exec_mode(compiled))
	 ; (print('Switching to Interactive mode'),nl,
	    set_exec_mode(interactive))
	).

/* ----------------------------------------------------- */

:- data make_iff_definitions_read_in/1.
make_iff_definitions_read_in(no).

set_make_iff_definitions_read_in(_Val) :-
	retract_fact(make_iff_definitions_read_in(_X)),fail.
set_make_iff_definitions_read_in(Val) :-
	assertz_fact(make_iff_definitions_read_in(Val)).

/* ---------------- */
/* output_to_file/1 */
/* ---------------- */

:- data output_to_file/1.
output_to_file(screen).

/* -------------------- */
/* set_output_to_file/0 */
/* -------------------- */

set_output_to_file_int(Val) :-
	(exec_mode(interactive) 
	->(print('Where should the specialised file be displayed:'),nl,
	   print('screen:   (display specialised program on screen)'),nl,
	   print('FileName: (write specialised program to file FileName)'),nl,
	   print('Current choice: '),
	   output_to_file(Cur),
	   print(Cur),nl,
	   print('choice =>'),read(NewValue))
	; NewValue = Val),
	set_output_to_file(NewValue),
	((NewValue = screen)
	-> (set_output_html(no))
	;  (set_html) /* allow user to choose whether html, yes or no */
	).

/* -------------------- */
/* set_output_to_file/1 */
/* -------------------- */

set_output_to_file(_NewVal) :-
	retract_fact(output_to_file(_Cur)),
	fail.
set_output_to_file(NewVal) :-
	asserta_fact(output_to_file(NewVal)).

/* ----------------------------------------------------- */

/* from file: code_generator.pro */

:- dynamic generate_slice_instead_of_spec_prog/1.
generate_slice_instead_of_spec_prog(no).


/* ----------------------------------------------------- */

/* from file: calc_chtree.pro */


/* :- use_module(library(clpfd)). */

:- data clpfd_active/1.
clpfd_active(no).

:- data rul_active/1.
rul_active(no).

constraints_active :- (clpfd_active(yes) ; rul_active(yes)).

:- dynamic detect_dead_literals_or_non_leftmost_builtins/1.
detect_dead_literals_or_non_leftmost_builtins(yes).

/* ----------------------------------------------------- */

/* from file: parametric_files.pro */

get_current_parameter_value(X,Y) :-
  (current_parameter_value(X,Y)
    -> true
    ; (print('### Warning: No current parameter value found !'),nl,
       print('### Call: '), print(get_current_parameter_value(X,Y)),nl
      )).
     
:- data current_parameter_value/2.
current_parameter_value(_X,_Y).

:- data perform_determinate_post_unfolding/1.
perform_determinate_post_unfolding(yes).
:- data perform_post_msv_analysis/1.
perform_post_msv_analysis(no).
:- data perform_raf/1.
perform_raf(yes).
:- data perform_far/1.
perform_far(yes).
:- data perform_dce/1.
perform_dce(yes).
:- data perform_parent_abstraction/1.
perform_parent_abstraction(yes).
:- data allow_removal_of_duplicate_predicates/1.
allow_removal_of_duplicate_predicates(yes). /* might change c.a.s. */

/* ------------------ */
/* update_parameter/2 */
/* ------------------ */

update_parameter(Parameter,_Char) :-
	retract_fact(current_parameter_value(Parameter,_C)),fail.
update_parameter(Parameter,Char) :-
	assertz_fact(current_parameter_value(Parameter,Char)).


/* ----------------------------------------------------- */

/* from file: raf-analysis.pro */

:- data dont_erase/3.
dont_erase(_Pred,_Arity,_Position).

:- data erasure_changed/0.
erasure_changed.

:- data var_call_encountered/0.
var_call_encountered.


reset_dont_erase :-
	retract_fact(dont_erase(_X,_Y,_Z)),fail.
reset_dont_erase.


reset_erasure_changed :-
	retract_fact(erasure_changed),fail.
reset_erasure_changed.


reset_var_call_encountered :-
	retract_fact(var_call_encountered),fail.
reset_var_call_encountered.

:- data gen_anonymous_vars_for_erased_args/1.

gen_anonymous_vars_for_erased_args(no).

/* ---------------- */
:- data nr_of_erased_arguments/1.

reset_nr_of_erased_arguments :- retract_fact(nr_of_erased_arguments(_X)),fail.
reset_nr_of_erased_arguments :- assertz_fact(nr_of_erased_arguments(0)).

inc_nr_of_erased_arguments :-
	retract_fact(nr_of_erased_arguments(Nr)), !, Nr1 is Nr + 1,
	assertz_fact(nr_of_erased_arguments(Nr1)).
inc_nr_of_erased_arguments :-
	assertz_fact(nr_of_erased_arguments(1)).



/* ----------------------------------------------------- */

/* from file: dead_code_elimination.pro */

:- data reachable/2.
reachable(_Pred,_Arity).

:- data reachable_changed/0.
reachable_changed.


reset_reachable :-
	retract_fact(reachable(_X,_Y)),fail.
reset_reachable.


reset_reachable_changed :-
	retract_fact(reachable_changed),fail.
reset_reachable_changed.

/* ----------------------------------------------------- */

/* from file: global_tree.pro */

:- data generate_dot_nodes_for_leaves/1.
generate_dot_nodes_for_leaves(yes).

set_generate_dot_nodes_for_leaves(NewVal) :-
   retract_fact(generate_dot_nodes_for_leaves(_)),
   assertz_fact(generate_dot_nodes_for_leaves(NewVal)).
   
