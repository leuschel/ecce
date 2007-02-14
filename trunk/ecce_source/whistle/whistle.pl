:- module(whistle,[whistle/4]).

/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,96,97,98  */
/* --------------------------------------------- */

/* TO GET THE LATEST VERSION OF THE ECCE SYSTEM GO TO:
	http://www.cs.kuleuven.ac.be/~michael */
/* BUG REPORTS, QUESTIONS AND COMMENTS CAN BE SENT TO:
	michael@cs.kuleuven.ac.be */

/* file: parameters.pro */

:- use_module('../dynpreds').
%:- dynamic whistle/4.
:- multifile available_options/4.
:- multifile available_ciaopp_option/3.

:- use_module('whistle.atomhomo.pl').
:- use_module('whistle.conjhomo.pl').
:- use_module('whistle.chconjhomo.pl').
:- use_module('whistle.chconjhomo-pf.pl').
:- use_module('whistle.chconj-termsize.pl').
:- use_module('whistle.chconj-termsize-unpruned.pl').
%:- use_module('whistle.chconj-termsize-set').
:- use_module('whistle.eco.pl').
:- use_module('whistle.homo.pl').
:- use_module('whistle.notmoregeneral.pl').
:- use_module('whistle.set.homo.pl').
:- use_module('whistle.none.pl').
:- use_module('whistle.conj-termsize.pl').
%:- use_module('whistle.chconj').
:- use_module('whistle.finkel.pl').
:- use_module('whistle.karp.pl').
%:- use_module('whistle.clpfd').


whistle(ID,G,Ch,W) :- 
   get_current_parameter_value(whistle,Z),
   'whistle:dispatch'(Z,ID,G,Ch,W).

'whistle:dispatch'(97,ID,G,Ch,W) :- 'whistle.atomhomo:whistle'(ID,G,Ch,W).
'whistle:dispatch'(99,ID,G,Ch,W) :- 'whistle.conjhomo:whistle'(ID,G,Ch,W).
'whistle:dispatch'(100,ID,G,Ch,W) :- 'whistle.chconjhomo:whistle'(ID,G,Ch,W).
'whistle:dispatch'(112,ID,G,Ch,W) :- 'whistle.chconjhomo-pf:whistle'(ID,G,Ch,W).
'whistle:dispatch'(102,ID,G,Ch,W) :- 'whistle.chconj-termsize:whistle'(ID,G,Ch,W).
'whistle:dispatch'(103,ID,G,Ch,W) :- 'whistle.chconj-termsize-unpruned:whistle'(ID,G,Ch,W).
%'whistle:dispatch'(106,ID,G,Ch,W) :- 'whistle.chconj-termsize-set':whistle'(ID,G,Ch,W).
'whistle:dispatch'(101,ID,G,Ch,W) :- 'whistle.eco:whistle'(ID,G,Ch,W).
'whistle:dispatch'(104,ID,G,Ch,W) :- 'whistle.homo:whistle'(ID,G,Ch,W).
'whistle:dispatch'(109,ID,G,Ch,W) :- 'whistle.notmoregeneral:whistle'(ID,G,Ch,W).
'whistle:dispatch'(115,ID,G,Ch,W) :- 'whistle.set.homo:whistle'(ID,G,Ch,W).
'whistle:dispatch'(117,ID,G,Ch,W) :- 'whistle.none:whistle'(ID,G,Ch,W).
'whistle:dispatch'(118,ID,G,Ch,W) :- 'whistle.conj-termsize:whistle'(ID,G,Ch,W).
%'whistle:dispatch'(119,ID,G,Ch,W) :- 'whistle.chconj':whistle(ID,G,Ch,W).
'whistle:dispatch'(110,ID,G,Ch,W) :- user_expert(yes),'whistle.finkel:whistle'(ID,G,Ch,W).
'whistle:dispatch'(111,ID,G,Ch,W) :- user_expert(yes),'whistle.karp:whistle'(ID,G,Ch,W).
%'whistle:dispatch'(98,ID,G,Ch,W) :- 'whistle.clpfd':whistle(ID,G,Ch,W).



/* ============ */
/*    WHISTLE   */
/* ============ */

available_ciaopp_option(whistle,99,'homeo').
available_ciaopp_option(whistle,118,'termsize').
available_ciaopp_option(whistle,102,'termsize-chtree').
available_ciaopp_option(whistle,100,'homeo-chtree').
available_ciaopp_option(whistle,109,'not-more-general').
available_ciaopp_option(whistle,117,'off').


available_options(whistle,97,
	'whistle/whistle.atomhomo.pl',
	'homeomorphic embedding whistle on Atoms only').
available_options(whistle,99,
	'whistle/whistle.conjhomo.pl',
	'homeomorphic embedding whistle on Conjunctions').
available_options(whistle,100,
	'whistle/whistle.chconjhomo.pl',
	'homeomorphic embedding whistle on (pruned+unpruned) Characteristic conjunctions').
available_options(whistle,112,
	'whistle/whistle.chconjhomo-pf.pl',
	'homeomorphic embedding whistle on (pruned+unpruned) Characteristic conjunctions + blow if pruning factor not sufficient').
available_options(whistle,102,
	'whistle/whistle.chconj-termsize.pl',
	'homeomorphic embedding whistle on characteristic trees + termsize on conjunctions').
available_options(whistle,103,
	'whistle/whistle.chconj-termsize-unpruned.pl',
	'homeomorphic embedding whistle on pruned+unpruned characteristic trees + termsize on conjunctions').
/* available_options(whistle,106,
	'whistle/whistle.chconj-termsize-set.pl',
	'set-based homeomorphic embedding whistle on characteristic trees + termsize on conjunctions').*/
available_options(whistle,101,
	'whistle/whistle.eco.pl',
	'Ecological partial deduction whistle
     (whistle if atom with same chtree and not strictly more general)
     (guarantees termination only if number of chtrees is finite!)').
available_options(whistle,104,
	'whistle/whistle.homo.pl',
	'Homeomorphic embedding whistle on atoms AND chtrees').
available_options(whistle,109,
	'whistle/whistle.notmoregeneral.pl',
	'blow if atom not More general than ancestor').
available_options(whistle,115,
	'whistle/whistle.set.homo.pl',
	'Set-based homeomorphic embedding whistle on atoms AND chtrees').
available_options(whistle,117,
	'whistle/whistle.none.pl',
	'No whistle').
available_options(whistle,118,
	'whistle/whistle.conj-termsize.pl',
	'just termsize on conjunctions').
/* available_options(whistle,119,
	'whistle/whistle.chconj.pl',
	'homeomorphic embedding whistle on characteristic trees +
        not strictly more general on conjunctions
		  (no longer available ?)'). */


available_options(whistle,110,
	'whistle/whistle.finkel.pl',
	'naive homeo on atoms (for Finkel algorithm)') :- 
	user_expert(yes).
available_options(whistle,111,
	'whistle/whistle.karp.pl',
	'naive homeo on atoms (for Karp-Miller algorithm)') :- 
	user_expert(yes).

/* available_options(whistle,98,
	'whistle/whistle.clpfd.pl',
	'whistle for CLP(FD)'). */
