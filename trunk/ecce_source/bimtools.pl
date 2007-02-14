:- module(bimtools,
	[
	    real/1,
	    project_and_check_constraint/3,
	    gensym/2,
	    gennum/1,
	    reset_gennum/1,
	    oldvalue/2,
	    set_flag/2,
	    msg/3,
	    next_free_clause_nr/2,
	    next_free_mode_nr/2,
	    claus/4,
	    declare_database/1,
	    undeclare_database/1,
	    print_claus_database_status/1,
	    cd/1,
	    clear_database/1,
	    read_database/0,
	    read_database/1,
	    read_database/2,
	    read_database/3,
	    add_new_clause/3,
	    treat_query/2,
	    add_new_mode/2,
	    add_clause/4,
	    claus_layout/2,
	    add_clause_with_layout/5,
	    comma_to_list/2,
	    make_mode_declaration/5,
	    make_iff_when_reading_clauses/1,
	    transform_clause/2,
	    set_make_iff_when_reading_clauses/0,
	    set_make_iff_when_reading_clauses/1,
	    using_ml_typechecker/1,
	    dont_assert/2,
	    strip_body/2,
	    strip_literal/2,
	    using_special_facts/1,
	    special_fact/1,
	    treat_special_fact/1,
	    claus/3,
	    mode_declaration/5,
	    next_free_clause_nr/1,
	    next_free_mode_nr/1,
	    print_claus_database_status/0,
	    cd/0,
	    clear_database/0,
	    rd/0,
	    add_new_clause/2,
	    treat_query/1,
	    add_new_mode/1,
	    add_clause/3,
	    exact_member/2,
	    member_nr/3,
	    split_list/5,
	    init_bd_findall/0,
	    bd_findall/3,
	    critical_bd_findall_nesting/0,
	    check_for_illegal_types/0,
	    print_type_definition_error/2,
	    predefined_type/1,
	    legal_type_description/1,
	    term_is_of_type/2,
	    term_is_of_type/3,
	    print_type_error/3,
	    print_inf_error/3,
	    safe_print_term/1,
	    assert_type_error/0,
	    reset_type_error/0,
	    type_error_occurred/0,
	    verify_pre/1,
	    verify_post/1,
	    pp_mnf/1,
	    pp_cll/1,
	    debug_printing/1,
	    set_debug_printing_value/1,
	    set_debug_printing/1,
	    debug_print/1,
	    debug_nl/0,
	    debug_check_point/1,
	    set_trace_printing_value/1,
	    set_trace_printing/1,
	    trace_print/1,trace_nl/0,
	    trace_check_point/1,
	    set_verbose_printing/1,
	    verbose_print/1, verbose_println/1, verbose_nl/0,
	    set_html/0,
	    set_output_html/1,
	    print_html/1,
	    print_command/2,
	    print_bold/1,
	    print_em/1,
	    print_yellow/1,
	    print_orange/1,
	    print_green/1,
	    print_red/1,
	    html_begin_yellow/0,
	    html_begin_orange/0,
	    html_begin_green/0,
	    html_end_color/0,
	    newparagraph/0,
	    newlinebreak/0,
	    ecce_put/1,
	    ecce_get/1,
        transform_dcg_term/2,
        please/2,
        rerecord/3,
        namevars/4,
        max/3,
        varlist/2,
        stop/0,
        time/2,
        time/1,
        instance_of/2,
        is_inf/1,
        variant_of/2,
        strict_instance_of/2,
	    read_term_with_lines/3
]).


% FOR CIAO!
% DTM: KEEP like this becaus the sicstus expansion has to eliminate them!

:- export( string_concatenate/3).
% :- export( transform_dcg_term/2).
% :- export( please/2).
% :- export( rerecord/3).
% :- export( namevars/4).
% :- export( max/3).
% :- export( varlist/2).
% :- export( stop/0).
% :- export( time/2).
% :- export( time/1).
:- export( copy/2).
:- export( on_exception/3).
:- export( expand_term/2).
:- export( ecce_source_directory/1).
:- export( ecce_benchmark_directory/1).
% :- export( instance_of/2).
% :- export( is_inf/1).
% :- export( variant_of/2).
% :- export( strict_instance_of/2).
:- export( same_length/2).

:- use_package( .(ecce_no_rt) ).

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(dec10_io)).
:- use_module(library(lists)).

:- use_module(library(aggregates)).


:-include( multi_meta ).
:- use_module(self_check).

%:- dynamic spec_clause/3.
%:- multifile spec_clause/3.

:- use_module(dynpreds).
:- use_module(homeomorphic).
:- use_module(global_tree).
:- use_module(calc_chtree).
%:- use_module(constraints).
:- use_module(determinate_post_unfold).
:- use_module(post_processor).
:- use_module(chtree_tools).
:- use_module(code_generator).
:- use_module(static_dynamic_functors).
:- use_module(depth_bound).
:- use_module(main_functions).
:- use_module(raf_analysis).

:- use_module('abstract/abstract').
:- use_module('check_instance_of/check_instance_of').
:- use_module('more_specific/more_specific').
:- use_module('neg_solve/neg_solve').
:- use_module('partition/partition').
:- use_module('selectionrule/selectionrule').
:- use_module('postprune/post_prune').
:- use_module('whistle/whistle').

:- use_module(unfold_history).

%:- use_module( engine(internals) , [term_to_meta/2,module_concat/3] ).

:- meta_predicate not( goal ).

not(Goal) :- \+(Goal).


:- include('bimtools/ciao_specific.pl').

:- include('bimtools/gensym.pl').
:- include('bimtools/msg.pl').
:- include('bimtools/claus_database.pl').
:- include('bimtools/instance.pl').
:- include('bimtools/stdlists.pl').
:- include('bimtools/bd_findall.pl').
:- include('bimtools/typechecker.pl').
:- include('bimtools/prepost.pl').
:- include('bimtools/debugging.pl').
:- include('bimtools/html_output.pl').
