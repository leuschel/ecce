:- module(more_specific,[more_specific_transformation/1]).

/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,96,97,98  */
/* --------------------------------------------- */

/* TO GET THE LATEST VERSION OF THE ECCE SYSTEM GO TO:
	http://www.cs.kuleuven.ac.be/~michael */
/* BUG REPORTS, QUESTIONS AND COMMENTS CAN BE SENT TO:
	michael@cs.kuleuven.ac.be */

/* file: parameters.pro */

:- use_module('../dynpreds').
%:- dynamic more_specific_transformation/1.
:- multifile available_options/4.

:- use_module('more_specific.functionality.pl').
%:- reexport('more_specific.functionality').
:- use_module('more_specific.none.pl').
%:- reexport('more_specific.none').
:- use_module('more_specific.simple.pl').
%:- reexport('more_specific.simple').
:- use_module('more_specific.simple-msv.pl').
%:- reexport('more_specific.simple-msv').
:- use_module('more_specific.clpfd.pl').
%:- reexport('more_specific.clpfd').
:- use_module('more_specific.conj-msv.pl').
%:- reexport('more_specific.conj-msv').

%dtm%
%jcf%
%:- export([unfold_once/2,get_foldable_instances/4]).
%unfold_once(A,B) :- 'more_specific.conj-msv':unfold_once(A,B).
%get_foldable_instances(A,B,C,D) :- 'more_specific.conj-msv':get_foldable_instances(A,B,C,D).
%jcf%
%dtm%

%%:- reexport( 'more_specific.conj-msv.pl', [unfold_once/2,get_foldable_instances/4]).


%:- use_module('more_specific.rul').


more_specific_transformation(G) :-
   get_current_parameter_value(msv,Z), 
   'more_specific:dispatch'(Z,G).

'more_specific:dispatch'(99 ,G) :- 'more_specific.conj-msv:more_specific_transformation'(G).
'more_specific:dispatch'(102,G) :- 'more_specific.functionality:more_specific_transformation'(G).
'more_specific:dispatch'(110,G) :- 'more_specific.none:more_specific_transformation'(G).
'more_specific:dispatch'(115,G) :- 'more_specific.simple:more_specific_transformation'(G).
'more_specific:dispatch'(116,G) :- 'more_specific.simple-msv:more_specific_transformation'(G).
'more_specific:dispatch'(118,G) :- 'more_specific.clpfd:more_specific_transformation'(G).
%'more_specific:dispatch'(119,G) :- 'more_specific.rul':more_specific_transformation(G).


/* ============ */
/*      MSV     */
/* ============ */
available_options(msv,99,
	'more_specific/more_specific.conj-msv.pl',
	'Conjunctive + msv of original program').
/* available_options(msv,100,
	'more_specific/more_specific.envunify.pl',
	'Simple, msg of matching heads + special functionality operations for env_unify predicates') :- 
	user_expert(yes). */
available_options(msv,102,
	'more_specific/more_specific.functionality.pl',
	'Simple, msg of matching heads + functionality based upon mode declarations (Warning: all modes supposed to be DETERMINATE for the output arguments ! e.g. :- mode f(i,o). means :- mode f(i,o) is determinate.)').
available_options(msv,110,
	'more_specific/more_specific.none.pl',
	'None').
available_options(msv,115,
	'more_specific/more_specific.simple.pl',
	'Simple, msg of matching heads').
available_options(msv,116,
	'more_specific/more_specific.simple-msv.pl',
	'Simple, msg of matching heads + msv of original program').
available_options(msv,118,
	'more_specific/more_specific.clpfd.pl',
	'Simple + check CLP(FD) satisfiability').
/* available_options(msv,119,
	'more_specific/more_specific.rul.pl',
	'Simple + check RUL satisfiability').
*/
