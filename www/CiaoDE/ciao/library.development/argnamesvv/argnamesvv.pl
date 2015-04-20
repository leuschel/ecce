%:- load_compilation_module(library('argnames/argnames_trans')).
:- load_compilation_module(library('argnamesvv/argnamesvv_trans')).
:- add_sentence_trans(argnames_def/3).
:- add_term_trans(argnames_use/3).
:- op(150, xfx, [$]).
:- op(950, xfx, (=>)).
:- op(1150, fx, [functor_class]).
