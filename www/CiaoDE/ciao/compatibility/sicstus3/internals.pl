/*             Copyright (C)1990-2002 UPM-CLIP				*/

%----------------------------------------------------------------------------
% Support CIAO builtins in standard Prolog systems (e.g., SICStus).
%----------------------------------------------------------------------------

:- module(internals, [
%        load_lib/2, load_so/2, dynlink/2,
%        '$atom_mode'/2, /* write.pl */
%        '$nodebug_call'/1,
%        '$bootversion'/0,
	'$open'/3,
%        '$purge'/1, '$erase'/1, '$ptr_ref'/2, '$inserta'/2,
%        '$insertz'/2, '$make_bytecode_object'/4, '$abolish'/1,
%        '$compile_term'/2,'$instance'/3,
%        '$define_predicate'/2, '$erase_clause'/1, '$clause_number'/2,
%        '$compiled_clause'/4, '$empty_gcdef_bin'/0, '$set_property'/2,
%        '$ddt'/1, '$qread'/2, '$push_qlinfo'/0, '$pop_qlinfo'/0,
%        '$prompt'/2, '$frozen'/2, '$defrost'/2, '$setarg'/4,
%        '$undo_goal'/1,
	'$metachoice'/1, '$metacut'/1,
%        '$retry_cut'/2,
%        '$exit'/1,
%        '$unknown'/2,
%        '$compiling'/2,
%        '$ferror_flag'/2,
%        '$quiet_flag'/2,
%        '$spypoint'/3, '$debugger_state'/2, '$debugger_mode'/0,
%        '$prolog_radix'/2, '$debugon'/0, '$constraint_list'/2, '$eq'/2,
%        '$large_data'/3, '$interpreted_clause'/2,
        /* unix.pl */
        '$unix_cd'/2,
%        '$unix_popen'/3,
%        '$unix_shell'/0, '$unix_shell'/2, '$unix_system'/2,
%        '$unix_argv'/1, '$unix_exit'/1, '$unix_mktemp'/2,
%        '$unix_access'/2, '$unix_chmod'/3,
%        '$unix_umask'/2,
        '$unix_modified'/2
%        '$load_foreign_files'/4, '$prepare_foreign_files'/3,
%        '$foreign_base'/1, '$find_file'/8,
%        '$format_print_float'/3, '$format_print_integer'/3, /* format.pl */
%        '$runtime'/1,
%        '$time'/1, '$termheap_usage'/1, '$envstack_usage'/1,
%        '$trail_usage'/1, '$choice_usage'/1, '$stack_shift_usage'/1,
%        '$program_usage'/1, '$total_usage'/1, '$gc_mode'/2, '$gc_trace'/2,
%        '$gc_margin'/2, '$gc_usage'/1, '$predicate_property'/3,
%        '$current_clauses'/2, '$first_instance'/2, '$current_instance'/5,
%        '$emulated_clause_counters'/4, '$counter_values'/3,
%        '$unlock_predicate'/1,
%        '$reset_counters'/2, poversion/1, undefined_goal/1,
%        initialization/1, on_abort/1,                  % implicit
%        imports/4, meta_args/2, multifile/3, defines/3 %
	]).

'$open'(Path, Mode, Stream):- open(Path, Mode, Stream).

'$metachoice'(Choice):- prolog:'$metachoice'(Choice).

'$metacut'(Choice):- prolog:'$metacut'(Choice).

'$unix_cd'(Old, New):- prolog:'$unix_cd'(Old, New).

'$unix_modified'(Path, Time):- prolog:'$unix_modified'(Path, Time).
