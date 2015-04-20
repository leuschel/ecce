
:- op(1150, fx, index).
:- new_declaration(index/1,off).

:- load_compilation_module(library('indexer/indexer_tr')).
:- add_sentence_trans(expand_index/3).

:- use_module(library('indexer/hash')).
