:- module(partition,[partition_goal/3]).

/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,96,97,98  */
/* --------------------------------------------- */

/* TO GET THE LATEST VERSION OF THE ECCE SYSTEM GO TO:
	http://www.cs.kuleuven.ac.be/~michael */
/* BUG REPORTS, QUESTIONS AND COMMENTS CAN BE SENT TO:
	michael@cs.kuleuven.ac.be */

/* file: parameters.pro */

:- use_module('../dynpreds').
:- dynamic partition_goal/3.
:- multifile available_options/4.
:- multifile available_ciaopp_option/3.

:- use_module('partition.conjnobi.pl').
:- use_module('partition.conjunctivepd.pl').
:- use_module('partition.staticconjnobi.pl').
:- use_module('partition.static-contig.pl').
:- use_module('partition.contig.pl').
:- use_module('partition.contig-bi.pl').
:- use_module('partition.standardpd.pl').
:- use_module('partition.staticcontig-mdes.pl').
:- use_module('partition.naive.pl').
:- use_module('partition.clpfd.pl').
%:- use_module('partition.rul').
%:- use_module('partition.rul-std').


partition_goal(L1,L2,G) :- 
   get_current_parameter_value(partition,Z),
   'partition:dispatch'(Z,L1,L2,G).

'partition:dispatch'(98,L1,L2,G) :- 'partition.conjnobi:partition_goal'(L1,L2,G).
'partition:dispatch'(99,L1,L2,G) :- 'partition.conjunctivepd:partition_goal'(L1,L2,G).
'partition:dispatch'(100,L1,L2,G) :- 'partition.staticconjnobi:partition_goal'(L1,L2,G).
'partition:dispatch'(101,L1,L2,G) :- 'partition.static-contig:partition_goal'(L1,L2,G).
'partition:dispatch'(102,L1,L2,G) :- 'partition.contig:partition_goal'(L1,L2,G).
'partition:dispatch'(103,L1,L2,G) :- 'partition.contig-bi:partition_goal'(L1,L2,G).
'partition:dispatch'(115,L1,L2,G) :- 'partition.standardpd:partition_goal'(L1,L2,G).
'partition:dispatch'(117,L1,L2,G) :- 'partition.staticcontig-mdes:partition_goal'(L1,L2,G).
'partition:dispatch'(110,L1,L2,G) :- user_expert(yes),'partition.naive:partition_goal'(L1,L2,G).
'partition:dispatch'(120,L1,L2,G) :- user_expert(yes),'partition.clpfd:partition_goal'(L1,L2,G).
%'partition:dispatch'(121,L1,L2,G) :- user_expert(yes),'partition.rul:partition_goal'(L1,L2,G).
%'partition:dispatch'(122,L1,L2,G) :- user_expert(yes),'partition.rul-std:partition_goal'(L1,L2,G).


/* ======================= */
/*        PARTITION        */
/* ======================= */

available_ciaopp_option(partition,115,'atoms').
available_ciaopp_option(partition,117,'conj-static').
available_ciaopp_option(partition,103,'conj-contig').
available_ciaopp_option(partition,99,'conj-max').


available_options(partition,98,
	'partition/partition.conjnobi.pl',
	'conjunctive PD (no built-ins inside conjunctions)').
available_options(partition,99,
	'partition/partition.conjunctivepd.pl',
	'Conjunctive PD (maximally connected sub-goals)').
available_options(partition,100,
	'partition/partition.staticconjnobi.pl',
	'conjunctive PD (only static conjunctions + no built-ins inside conjunctions)').
available_options(partition,101,
	'partition/partition.static-contig.pl',
	'conjunctive PD (only static contiguous conjunctions, no built-ins)').
available_options(partition,102,
	'partition/partition.contig.pl',
	'conjunctive PD (only contiguous conjunctions, no built-ins)').
available_options(partition,103,
	'partition/partition.contig-bi.pl',
	'conjunctive PD (only contiguous conjunctions, with built-ins)').
available_options(partition,115,
	'partition/partition.standardpd.pl',
	'Standard PD (partition into atomic sub-goals)').
available_options(partition,117,
	'partition/partition.staticcontig-mdes.pl',
	'conjunctive PD (only static contiguous conjunctions, no built-ins, require i/o mode link)').

available_options(partition,110,
	'partition/partition.naive.pl',
	'naive standard PD (only atoms, make non-ground args fully dynamic)') :- 
	user_expert(yes).

available_options(partition,120,
	'partition/partition.clpfd.pl',
	'conj. PD (only contiguous conj., with built-ins) + project CLP(FD) constraints') :- 
	user_expert(yes).
/* available_options(partition,121,
	'partition/partition.rul.pl',
	'conj. PD (only contiguous conj., with built-ins) + project RUL constraints') :- 
	user_expert(yes).
available_options(partition,122,
	'partition/partition.rul-std.pl',
	'std PD  + project RUL constraints') :- 
	user_expert(yes). 
*/

