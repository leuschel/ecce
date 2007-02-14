:- module(abstract,[abstract_parent/6,abstract_leaf/6]).

/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,96,97,98  */
/* --------------------------------------------- */

/* TO GET THE LATEST VERSION OF THE ECCE SYSTEM GO TO:
	http://www.cs.kuleuven.ac.be/~michael */
/* BUG REPORTS, QUESTIONS AND COMMENTS CAN BE SENT TO:
	michael@cs.kuleuven.ac.be */

/* file: parameters.pro */

:- use_module('../dynpreds').

:- use_module('../calc_chtree').
%:- dynamic abstract_parent/6.
%:- dynamic abstract_leaf/6.
:- multifile available_options/4.
:- multifile available_ciaopp_option/3.

:- use_module('abstract.msgchtree.pl').
:- use_module('abstract.precisemsgchtree.pl').
:- use_module('abstract.eco.pl').
:- use_module('abstract.conjsplit-contig.pl').
:- use_module('abstract.conjsplit.pl').
:- use_module('abstract.conjsplit-termsize.pl').
:- use_module('abstract.conjsplit-tszcntig.pl').
:- use_module('abstract.msg.pl').
:- use_module('abstract.naivemsg.pl').
%:- use_module('abstract.clpfd').
%:- use_module('abstract.rul').  (connected to 'rul/analyticFold')
%:- use_module('abstract.rul-conjcntg').

abstract_parent(ID,G,Ch,W,NewG,NewCh) :- 
   get_current_parameter_value(abstraction,Z),
   'abstract:dispatch'(parent,Z,ID,G,Ch,W,NewG,NewCh).

abstract_leaf(ID,G,Ch,W,NewG,NewCh) :- 
   get_current_parameter_value(abstraction,Z),
   'abstract:dispatch'(leaf,Z,ID,G,Ch,W,NewG,NewCh).

'abstract:dispatch'(parent,99,ID,G,Ch,W,NewG,NewCh) :- 'abstract.msgchtree:abstract_parent'(ID,G,Ch,W,NewG,NewCh).
'abstract:dispatch'(leaf,99,ID,G,Ch,W,NewG,NewCh) :- 'abstract.msgchtree:abstract_leaf'(ID,G,Ch,W,NewG,NewCh).
'abstract:dispatch'(parent,100,ID,G,Ch,W,NewG,NewCh) :- 'abstract.precisemsgchtree:abstract_parent'(ID,G,Ch,W,NewG,NewCh).
'abstract:dispatch'(leaf,100,ID,G,Ch,W,NewG,NewCh) :- 'abstract.precisemsgchtree:abstract_leaf'(ID,G,Ch,W,NewG,NewCh).
'abstract:dispatch'(parent,101,ID,G,Ch,W,NewG,NewCh) :- 'abstract.eco:abstract_parent'(ID,G,Ch,W,NewG,NewCh).
'abstract:dispatch'(leaf,101,ID,G,Ch,W,NewG,NewCh) :- 'abstract.eco:abstract_leaf'(ID,G,Ch,W,NewG,NewCh).
'abstract:dispatch'(parent,105,ID,G,Ch,W,NewG,NewCh) :- 'abstract.conjsplit-contig:abstract_parent'(ID,G,Ch,W,NewG,NewCh).
'abstract:dispatch'(leaf,105,ID,G,Ch,W,NewG,NewCh) :- 'abstract.conjsplit-contig:abstract_leaf'(ID,G,Ch,W,NewG,NewCh).
'abstract:dispatch'(parent,106,ID,G,Ch,W,NewG,NewCh) :- 'abstract.conjsplit:abstract_parent'(ID,G,Ch,W,NewG,NewCh).
'abstract:dispatch'(leaf,106,ID,G,Ch,W,NewG,NewCh) :- 'abstract.conjsplit:abstract_leaf'(ID,G,Ch,W,NewG,NewCh).
'abstract:dispatch'(parent,107,ID,G,Ch,W,NewG,NewCh) :- 'abstract.conjsplit-termsize:abstract_parent'(ID,G,Ch,W,NewG,NewCh).
'abstract:dispatch'(leaf,107,ID,G,Ch,W,NewG,NewCh) :- 'abstract.conjsplit-termsize:abstract_leaf'(ID,G,Ch,W,NewG,NewCh).
'abstract:dispatch'(parent,108,ID,G,Ch,W,NewG,NewCh) :- 'abstract.conjsplit-tszcntig:abstract_parent'(ID,G,Ch,W,NewG,NewCh).
'abstract:dispatch'(leaf,108,ID,G,Ch,W,NewG,NewCh) :- 'abstract.conjsplit-tszcntig:abstract_leaf'(ID,G,Ch,W,NewG,NewCh).
'abstract:dispatch'(parent,109,ID,G,Ch,W,NewG,NewCh) :- 'abstract.msg:abstract_parent'(ID,G,Ch,W,NewG,NewCh).
'abstract:dispatch'(leaf,109,ID,G,Ch,W,NewG,NewCh) :- 'abstract.msg:abstract_leaf'(ID,G,Ch,W,NewG,NewCh).
'abstract:dispatch'(parent,110,ID,G,Ch,W,NewG,NewCh) :- user_expert(yes),'abstract.naivemsg:abstract_parent'(ID,G,Ch,W,NewG,NewCh).
'abstract:dispatch'(leaf,110,ID,G,Ch,W,NewG,NewCh) :- user_expert(yes),'abstract.naivemsg:abstract_leaf'(ID,G,Ch,W,NewG,NewCh).
%'abstract:dispatch'(parent,111,ID,G,Ch,W,NewG,NewCh) :- user_expert(yes),'abstract.clpfd:abstract_parent'(ID,G,Ch,W,NewG,NewCh).
%'abstract:dispatch'(leaf,111,ID,G,Ch,W,NewG,NewCh) :- user_expert(yes),'abstract.clpfd:abstract_leaf'(ID,G,Ch,W,NewG,NewCh).
%'abstract:dispatch'(parent,112,ID,G,Ch,W,NewG,NewCh) :- user_expert(yes),'abstract.rul:abstract_parent'(ID,G,Ch,W,NewG,NewCh).
%'abstract:dispatch'(leaf,112,ID,G,Ch,W,NewG,NewCh) :- user_expert(yes),'abstract.rul:abstract_leaf'(ID,G,Ch,W,NewG,NewCh).
%'abstract:dispatch'(parent,113,ID,G,Ch,W,NewG,NewCh) :- user_expert(yes),'abstract.rul-conjcntg:abstract_parent'(ID,G,Ch,W,NewG,NewCh).
%'abstract:dispatch'(leaf,113,ID,G,Ch,W,NewG,NewCh) :- user_expert(yes),'abstract.rul-conjcntg:abstract_leaf'(ID,G,Ch,W,NewG,NewCh).


/* ================ */
/*    ABSTRACTION   */
/* ================ */

available_ciaopp_option(abstraction,109,'msg').
available_ciaopp_option(abstraction,110,'naive').
available_ciaopp_option(abstraction,108,'conj-msg').

available_options(abstraction,99,
	'abstract/abstract.msgchtree.pl',
	'msg of atoms and Chtree part
     (impose Msg of chtree part on the generalisation)').
available_options(abstraction,100,
	'abstract/abstract.precisemsgchtree.pl',
	'msg of atoms and precise msg of chtree part
     (impose Msg of chtree part on the generalisation)').
available_options(abstraction,101,
	'abstract/abstract.eco.pl',
	'Ecological Partial Deduction
     (Msg of atoms with same chtree + impose chtree)').
available_options(abstraction,105,
	'abstract/abstract.conjsplit-contig.pl',
	'conJunctive split: most specific msg with homeomorphic embedded contiguous subconjunction (or split off first literal if none exists)').
available_options(abstraction,106,
	'abstract/abstract.conjsplit.pl',
	'conJunctive split: most specific msg with homeomorphic embedded subconjunction').
available_options(abstraction,107,
	'abstract/abstract.conjsplit-termsize.pl',
	'conjunctive split: most specific msg with term-size-embedded subconjunction').
available_options(abstraction,108,
	'abstract/abstract.conjsplit-tszcntig.pl',
	'conjunctive split: most specific msg with term-size-embedded contiguous subconjunction (or split off first literal if none exists)').
available_options(abstraction,109,
	'abstract/abstract.msg.pl',
	'Msg of atom part (impose nothing on generalisation)').
available_options(abstraction,110,
	'abstract/abstract.naivemsg.pl',
	'Naive Msg of atom part (impose nothing; for KarpMiller/Finkel Algo)')
:- user_expert(yes).
/* available_options(abstraction,111,
	'abstract/abstract.clpfd.pl',
	'Msg of atom part + widening of CLP(FD) constraints')
   :- user_expert(yes).
available_options(abstraction,112,
	'abstract/abstract.rul.pl',
	'shortening of RUL constraints, no splitting'):- user_expert(yes).
available_options(abstraction,113,
	'abstract/abstract.rul-conjcntg.pl',
	'conj split: most specific msg with predicate embedded contig subconj + shortening of RUL constraints'):- user_expert(yes).
*/

