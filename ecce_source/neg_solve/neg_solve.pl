:- module(neg_solve,[neg_solve/2]).

/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,96,97,98  */
/* --------------------------------------------- */

/* TO GET THE LATEST VERSION OF THE ECCE SYSTEM GO TO:
	http://www.cs.kuleuven.ac.be/~michael */
/* BUG REPORTS, QUESTIONS AND COMMENTS CAN BE SENT TO:
	michael@cs.kuleuven.ac.be */

/* file: parameters.pro */

:- use_module('../dynpreds').
:- dynamic neg_solve/2.
:- multifile available_options/4.

:- use_module('neg_solve.any.pl').
:- use_module('neg_solve.ground.pl').
:- use_module('neg_solve.none.pl').
:- use_module('neg_solve.always.pl').


neg_solve(G,SS) :- 
   get_current_parameter_value(negsolve,Z),
   'neg_solve:dispatch'(Z,G,SS).

'neg_solve:dispatch'(97,G,SS) :- 'neg_solve.any:neg_solve'(G,SS).
'neg_solve:dispatch'(103,G,SS) :- 'neg_solve.ground:neg_solve'(G,SS).
'neg_solve:dispatch'(110,G,SS) :- 'neg_solve.none:neg_solve'(G,SS).
'neg_solve:dispatch'(122,G,SS) :- user_expert(yes),'neg_solve.always:neg_solve'(G,SS).


/* ======================= */
/*      NEGATIVE SOLVE     */
/* ======================= */
available_options(negsolve,97,
	'neg_solve/neg_solve.any.pl',
	'solve ground + unground if empty c.a.s.').
available_options(negsolve,103,
	'neg_solve/neg_solve.ground.pl',
	'solve ground (or dead) negative literals
     (supposes ground negative literals terminate)').
available_options(negsolve,110,
	'neg_solve/neg_solve.none.pl',
	'none').
available_options(negsolve,122,
	'neg_solve/neg_solve.always.pl',
	'always (be careful about what you are doing !!)') :- 
	user_expert(yes).

