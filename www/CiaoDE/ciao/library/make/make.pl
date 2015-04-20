% The Ciao make package
% Documentation in make_doc.pl
% See also the lpmake application

:- include(library('make/make_ops')).
:- include(library('make/make_com')).

:- discontiguous do_target/1.
:- discontiguous target_exists/1.
:- discontiguous target_deps/2.
:- discontiguous target_comment/3.
:- discontiguous do_dependency/3.
:- discontiguous dependency_exists/2.
:- discontiguous dependency_precond/3.

:- load_compilation_module(library('make/make_tr')).
:- add_sentence_trans(defdep/3).

:- use_module(library('make/make_rt'),
              [make/1,verbose_message/2,make_option/1,
	       call_unknown/1,
	       all_values/2,
	       dot_concat/2,
	       get_value/2,
	       get_value_def/3,
	       find_file/2,
	       bold_message/1,
	       bold_message/2,
	       dyn_load_cfg_module_into_make/1]).
