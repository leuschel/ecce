:- module('more_specific.simple-msv',['more_specific.simple-msv:more_specific_transformation'/1]).

%:- dynamic more_specific_transformation/1.

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../msv_analysis').

:- use_module( mst ).

/* instantiates goals during the unfolding process to more specific versions */

'more_specific.simple-msv:more_specific_transformation'(Goal) :-
	msv_of_goal(Goal),
	more_specific_transformation2(Goal).

more_specific_transformation2([]).
more_specific_transformation2([CH|T]) :-
	peel_off_calls(CH,H),
	mst_instantiate_atom(H),!,
	more_specific_transformation2(T).
