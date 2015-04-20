:- module(check_instance_of,[get_instance_of/4]).

/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,96,97,98  */
/* --------------------------------------------- */

/* TO GET THE LATEST VERSION OF THE ECCE SYSTEM GO TO:
	http://www.cs.kuleuven.ac.be/~michael */
/* BUG REPORTS, QUESTIONS AND COMMENTS CAN BE SENT TO:
	michael@cs.kuleuven.ac.be */

/* file: parameters.pro */

:- use_module('../dynpreds').
% :- dynamic get_instance_of/4.
:- multifile available_options/4.
:- multifile available_ciaopp_option/3.

:- use_module('check_instance_of.instance.pl').
:- use_module('check_instance_of.instchtr.pl').
:- use_module('check_instance_of.variant.pl').
:- use_module('check_instance_of.karp.pl').
%:- use_module('check_instance_of.clpfd').
%:- use_module('check_instance_of.rul').


get_instance_of(ID,G,Ch,MG) :-
   get_current_parameter_value(instchecks,Z),
   'check_instance_of:dispatch'(Z,ID,G,Ch,MG).

'check_instance_of:dispatch'(97 ,ID,G,Ch,MG) :- 'check_instance_of.instance:get_instance_of'(ID,G,Ch,MG).
'check_instance_of:dispatch'(105,ID,G,Ch,MG) :- 'check_instance_of.instchtr:get_instance_of'(ID,G,Ch,MG).
'check_instance_of:dispatch'(118,ID,G,Ch,MG) :- 'check_instance_of.variant:get_instance_of'(ID,G,Ch,MG).
'check_instance_of:dispatch'(121,ID,G,Ch,MG) :- user_expert(yes),'check_instance_of.karp:get_instance_of'(ID,G,Ch,MG).
%'check_instance_of:dispatch'(99,ID,G,Ch,MG) :- user_expert(yes),'check_instance_of.clpfd':get_instance_of(ID,G,Ch,MG).
%'check_instance_of:dispatch'(100,ID,G,Ch,MG) :- user_expert(yes),'check_instance_of.rul':get_instance_of(ID,G,Ch,MG).


/* =================== */
/* CHECK FOR INSTANCES */
/* =================== */

available_ciaopp_option(instchecks,97,'instance').
available_ciaopp_option(instchecks,105,'instance-chtree').
available_ciaopp_option(instchecks,118,'variant-chtree').


available_options(instchecks,97,
	'check_instance_of/check_instance_of.instance.pl',
	'Any instance of a goal').
available_options(instchecks,105,
	'check_instance_of/check_instance_of.instchtr.pl',
	'Instance of a goal with same chtree').
available_options(instchecks,118,
	'check_instance_of/check_instance_of.variant.pl',
	'Variant of a goal with same chtree').
available_options(instchecks,121,
	'check_instance_of/check_instance_of.karp.pl',
	'Variant of a goal with same chtree + slight variation for Karp Miller')
 :- user_expert(yes).
/* available_options(instchecks,99,
	'check_instance_of/check_instance_of.clpfd.pl',
	'any instance for CLP(FD)')
 :- user_expert(yes).
available_options(instchecks,100,
	'check_instance_of/check_instance_of.rul.pl',
	'any instance for RUL domain')
 :- user_expert(yes).
*/

