:- module('more_specific.clpfd',['more_specific.clpfd:more_specific_transformation'/1]).

%:- dynamic more_specific_transformation/1.

:- set_prolog_flag(multi_arity_warnings,off).
%:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../constraints/constraints_clpfd',[project_and_check_constraint/3]).

:- use_module( mst ).

/* instantiates goals during the unfolding process to more specific versions */

'more_specific.clpfd:more_specific_transformation'(Goal) :-
	divide_constraint_goal(Goal,OGoal,CGoal),
	more_specific_transformation_classic(OGoal),
	project_and_check_constraint(OGoal,CGoal,_).

more_specific_transformation_classic([]).
more_specific_transformation_classic([CH|T]) :-
	peel_off_calls(CH,H),
	mst_instantiate_atom(H),!,
	more_specific_transformation_classic(T).

	
