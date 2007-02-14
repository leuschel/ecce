
/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,96,97,98  */
/*     [Ported to Ciao by Mauricio Varea, 2003]  */
/* --------------------------------------------- */

/* TO GET THE LATEST VERSION OF THE ECCE SYSTEM GO TO:
	http://www.cs.kuleuven.ac.be/~michael */
/* BUG REPORTS, QUESTIONS AND COMMENTS CAN BE SENT TO:
	michael@cs.kuleuven.ac.be */

:- use_module(library(dec10_io)).

:- include( multi_meta ).


/* file: ecce_main.pro */


:- use_module(dynpreds).
:- use_module(bimtools).


:- use_module(determinate_post_unfold).
:- use_module(unfold_helper).

:- use_module(main_functions).

/* :- use_module('constraints'). */
ecce_type(constraint,list(elementary_constraint)).
ecce_type(constraint,term(fail,[])).
ecce_type(elementary_constraint,term(ecce_type,[nonvar,any])).
ecce_type(simplified_constraint,list(elementary_simplified_constraint)).
ecce_type(elementary_simplified_constraint,term(ecce_type,[nonvar,var])).


:- use_module(parametric_files).
:- use_module(self_check).


:- include('front_end.pl').

%:- initialization(perform_self_check).
%:- initialization(initialise_parameters).
