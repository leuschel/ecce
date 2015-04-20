:- load_compilation_module(library(dcg_expansion)).
:- add_sentence_trans(dcg_translation/2).
:- op(1200, xfx,[(-->)]).
:- op(1100, xfy, ('|')).
