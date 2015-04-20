:- use_module(library('andorra/andorra_rt'),[wakeup/2,
	                  suspend_andorra/5, 
			  obtain_vars/2,
			  verify_det/3,
			  simplify/2]).

:- use_module(library('andorra/andorra_builtins')).

:- use_module(library(terms_vars),[varset/2]).

:- include(library('andorra/andorraops')).

:- load_compilation_module(library('andorra/andorra_tr')).

:- add_sentence_trans(translation1/3).
:- add_clause_trans(translation2/3).
