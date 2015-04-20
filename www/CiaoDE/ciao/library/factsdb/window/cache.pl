
:- load_compilation_module(library('cache/cache_tr')).
:- add_sentence_trans(cache_exp/2).
:- use_module(library('cache/cache_rt'),[cache_call/3]).
:- multifile '$cache$cached_goal'/4.
:- discontiguous '$cache$cached_goal'/4.
