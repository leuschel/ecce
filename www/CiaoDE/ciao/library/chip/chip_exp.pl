
:- module(chip_exp,
	[ expand_chip/2 ],
	[ ]).

expand_chip((:- consult(F)),(:- include(F))).
expand_chip((:- reconsult(F)),(:- include(F))).
expand_chip((:- lib(chipre_decl)),(:- comment(lib,chipre_decl))):- !.
expand_chip((:- lib(F)),(:- ensure_loaded(library(F)))).
expand_chip((?- D),(:- E)):- expand_chip((:- D),(:- E)).
