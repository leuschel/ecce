:- module('more_specific.functionality',['more_specific.functionality:more_specific_transformation'/1]).

%:- dynamic more_specific_transformation/1.

/* WARNING: supposes all mode declarations mean that the call
   satisfying the modes is DETERMINATE wrt output arguments*/

:- set_prolog_flag(multi_arity_warnings,off).
%:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../modes').

:- use_package( .('../ecce_no_rt2') ).

:- use_module( mst ).


/* instantiates goals during the unfolding process to more specific versions */

'more_specific.functionality:more_specific_transformation'([]).
'more_specific.functionality:more_specific_transformation'([CH|T]) :-
	peel_off_calls(CH,H),
	mst_instantiate_atom(H),!,
	functionality_msv(H,T),!,
	'more_specific.functionality:more_specific_transformation'(T).


/* use determinism mode declarations to implement functionality inference: */
functionality_msv(H,T) :-
	nonvar(H),
	not(is_negative_literal(H,_Atom)),
	find_applicable_mode_declaration(H,InArgs,AnyArgs,OutArgs,
				         DCall,DI,DO),!,
		/* several might apply: future work: try all */
	((member(DCall,T), DI==InArgs) 
	 -> (DO = OutArgs,print(f),
	    debug_print(func(Pred,DI,DO)),debug_nl)
	 ;  true
	).
functionality_msv(H,T).

