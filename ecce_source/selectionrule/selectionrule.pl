:- module(selectionrule,[select_positive_literal/5]).

/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,96,97,98  */
/* --------------------------------------------- */

/* TO GET THE LATEST VERSION OF THE ECCE SYSTEM GO TO:
	http://www.cs.kuleuven.ac.be/~michael */
/* BUG REPORTS, QUESTIONS AND COMMENTS CAN BE SENT TO:
	michael@cs.kuleuven.ac.be */

/* file: parameters.pro */

:- multifile available_options/4.
:- multifile available_ciaopp_option/3.


:- use_module('../dynpreds').
:- dynamic select_positive_literal/5.

:- use_module('selectionrule.det-and-once.pl').
:- use_module('selectionrule.once.pl').
:- use_module('selectionrule.predonce.pl').
:- use_module('selectionrule.det.pl').
:- use_module('selectionrule.homo.pl').
:- use_module('selectionrule.homo-leftmost.pl').
:- use_module('selectionrule.homo-pure.pl').
:- use_module('selectionrule.dethomo-idx.pl').
:- use_module('selectionrule.dethomo.pl').
:- use_module('selectionrule.detbihomo.pl').
:- use_module('selectionrule.detunsafehomo.pl').
:- use_module('selectionrule.1nondet.pl').
:- use_module('selectionrule.1nondet.unsfe.pl').
:- use_module('selectionrule.safedet.pl').
:- use_module('selectionrule.any-det.pl').
:- use_module('selectionrule.any-det-facts.pl').
:- use_module('selectionrule.mixtus.pl').
:- use_module('selectionrule.mixtus-det.pl').
:- use_module('selectionrule.sdr.pl').
:- use_module('selectionrule.term-det.pl').


select_positive_literal(G,TopG,UH,NoS,SL) :- 
   get_current_parameter_value(selectionrule,Z),
   'selectionrule:dispatch'(Z,G,TopG,UH,NoS,SL).

'selectionrule:dispatch'(98,G,TopG,UH,NoS,SL) :- 'selectionrule.det-and-once:select_positive_literal'(G,TopG,UH,NoS,SL).
'selectionrule:dispatch'(99,G,TopG,UH,NoS,SL) :- user_expert(yes),'selectionrule.once:select_positive_literal'(G,TopG,UH,NoS,SL).
'selectionrule:dispatch'(101,G,TopG,UH,NoS,SL) :- user_expert(yes),'selectionrule.predonce:select_positive_literal'(G,TopG,UH,NoS,SL).
'selectionrule:dispatch'(100,G,TopG,UH,NoS,SL) :- user_expert(yes),'selectionrule.det:select_positive_literal'(G,TopG,UH,NoS,SL).
'selectionrule:dispatch'(104,G,TopG,UH,NoS,SL) :- 'selectionrule.homo:select_positive_literal'(G,TopG,UH,NoS,SL).
'selectionrule:dispatch'(105,G,TopG,UH,NoS,SL) :- 'selectionrule.homo-pure:select_positive_literal'(G,TopG,UH,NoS,SL).
'selectionrule:dispatch'(106,G,TopG,UH,NoS,SL) :- 'selectionrule.homo-leftmost:select_positive_literal'(G,TopG,UH,NoS,SL).
'selectionrule:dispatch'(107,G,TopG,UH,NoS,SL) :- 'selectionrule.dethomo-idx:select_positive_literal'(G,TopG,UH,NoS,SL).
'selectionrule:dispatch'(108,G,TopG,UH,NoS,SL) :- 'selectionrule.dethomo:select_positive_literal'(G,TopG,UH,NoS,SL).
'selectionrule:dispatch'(109,G,TopG,UH,NoS,SL) :- 'selectionrule.detbihomo:select_positive_literal'(G,TopG,UH,NoS,SL).
'selectionrule:dispatch'(113,G,TopG,UH,NoS,SL) :- user_expert(yes),'selectionrule.detunsafehomo:select_positive_literal'(G,TopG,UH,NoS,SL).
'selectionrule:dispatch'(111,G,TopG,UH,NoS,SL) :- 'selectionrule.1nondet:select_positive_literal'(G,TopG,UH,NoS,SL).
'selectionrule:dispatch'(112,G,TopG,UH,NoS,SL) :- user_expert(yes),'selectionrule.1nondet.unsfe:select_positive_literal'(G,TopG,UH,NoS,SL).
'selectionrule:dispatch'(115,G,TopG,UH,NoS,SL) :- 'selectionrule.safedet:select_positive_literal'(G,TopG,UH,NoS,SL).
'selectionrule:dispatch'(116,G,TopG,UH,NoS,SL) :- 'selectionrule.any-det:select_positive_literal'(G,TopG,UH,NoS,SL).
'selectionrule:dispatch'(117,G,TopG,UH,NoS,SL) :- 'selectionrule.any-det-facts:select_positive_literal'(G,TopG,UH,NoS,SL).
'selectionrule:dispatch'(120,G,TopG,UH,NoS,SL) :- 'selectionrule.mixtus:select_positive_literal'(G,TopG,UH,NoS,SL).
'selectionrule:dispatch'(121,G,TopG,UH,NoS,SL) :- 'selectionrule.mixtus-det:select_positive_literal'(G,TopG,UH,NoS,SL).
'selectionrule:dispatch'(122,G,TopG,UH,NoS,SL) :- user_expert(yes),'selectionrule.sdr:select_positive_literal'(G,TopG,UH,NoS,SL).
'selectionrule:dispatch'(114,G,TopG,UH,NoS,SL) :- user_expert(yes),
   'selectionrule.term-det:select_positive_literal'(G,TopG,UH,NoS,SL).


/* =================== */
/*    SELECTION RULE   */
/* =================== */

available_ciaopp_option(selectionrule,115,'atoms').
available_ciaopp_option(selectionrule,117,'conj-static').
available_ciaopp_option(selectionrule,103,'conj-contig').
available_ciaopp_option(selectionrule,99,'conj-max').


available_options(selectionrule,98,
	'selectionrule/selectionrule.det-and-once.pl',
	'any det + once (t+c)').
available_options(selectionrule,99,
	'selectionrule/selectionrule.once.pl',
	'only onCe') :- 
	user_expert(yes).
available_options(selectionrule,101,
	'selectionrule/selectionrule.predonce.pl',
	'every predicate only onCe (test with covering ancestors)') :- 
	user_expert(yes).
available_options(selectionrule,100,
	'selectionrule/selectionrule.det.pl',
	'Determinate unfolding (unsafe w/o depth bound, non-determinate steps only allowed at top)') :- 
	user_expert(yes).
available_options(selectionrule,104,
	'selectionrule/selectionrule.homo.pl',
	'Homeomorphic unfolding (test with covering ancestors) + require search space reduction + do not unfold atomic goals').
available_options(selectionrule,105,
	'selectionrule/selectionrule.homo-pure.pl',
	'pure homeomorphic unfolding (test with covering ancestors)').
available_options(selectionrule,106,
	'selectionrule/selectionrule.homo-leftmost.pl',
	'pure *leftmost only* homeomorphic unfolding - no non-leftmost unfolding (even determinate)').
available_options(selectionrule,107,
	'selectionrule/selectionrule.dethomo-idx.pl',
	'determinate (first) + Leftmost non-determinate, indexed homeo. unfolding').
available_options(selectionrule,108,
	'selectionrule/selectionrule.dethomo.pl',
	'determinate (first) + Leftmost non-determinate homeo. unfolding').
available_options(selectionrule,109,
	'selectionrule/selectionrule.detbihomo.pl',
	'option l + allow jump over built-ins for non-determinate unfoldings').
available_options(selectionrule,113,
	'selectionrule/selectionrule.detunsafehomo.pl',
	'option l + do not check determinate steps for non-termination (=> method is potentially unsafe)') :- 
	user_expert(yes).

available_options(selectionrule,111,
	'selectionrule/selectionrule.1nondet.pl',
	'allow One left-most non-determinate step homeomorphic unfolding').
available_options(selectionrule,112,
	'selectionrule/selectionrule.1nondet.unsfe.pl',
	'allow one left-most non-determinate step (w/o homeo. => unsafe w/o depth bound)') :- 
	user_expert(yes).
available_options(selectionrule,115,
	'selectionrule/selectionrule.safedet.pl',
	'Safe determinate unfolding (using homeomorphic embedding)').
available_options(selectionrule,116,
	'selectionrule/selectionrule.any-det.pl',
	'any (safe) determinate unfolding').
available_options(selectionrule,117,
	'selectionrule/selectionrule.any-det-facts.pl',
	'any (safe) determinate unfolding + fact unfolding').
available_options(selectionrule,120,
	'selectionrule/selectionrule.mixtus.pl',
	'miXtus like unfolding rule (only leftmost unfolding [allow jump over built-ins])').
available_options(selectionrule,121,
	'selectionrule/selectionrule.mixtus-det.pl',
	'mixtus like unfolding rule (leftmost unfolding + determinate unfolding)').
available_options(selectionrule,122,
	'selectionrule/selectionrule.sdr.pl',
	'synchronised descent rule (as def. by Proietti/Pettorossi)') :- 
	user_expert(yes).


available_options(selectionrule,114,
	'selectionrule/selectionrule.term-det.pl',
	'pure *leftmost only* homeomorphic unfolding - only 1 non-det step') :- 
	user_expert(yes).