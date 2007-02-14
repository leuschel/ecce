:- module('neg_solve.always',['neg_solve.always:neg_solve'/2]).

%:- dynamic neg_solve/2.

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').

/* supposes that evaluation of negated literals will terminate */

'neg_solve.always:neg_solve'([],success) :- !.
'neg_solve.always:neg_solve'(Goal,fail) :-
	dead(Goal),!.
'neg_solve.always:neg_solve'([H|_T],SolveSol) :-
	is_negative_literal(H,Atom),!,
	'neg_solve.always:neg_solve'([Atom],AtomSol),
	((AtomSol=fail)
	 -> (SolveSol = success)
	 ;  ((AtomSol=success)
	      -> (SolveSol = fail)
	      ;  (SolveSol = unknown)
	    )
	).
'neg_solve.always:neg_solve'([H|T],SolveSol) :-
	is_built_in_literal(H),!,
	(is_callable_built_in_literal(H)
	 -> (call_built_in(H)
		-> ('neg_solve.always:neg_solve'(T,SolveSol))
		;  (SolveSol = fail)
	    )
	 ;  (SolveSol = unknown)
	).
'neg_solve.always:neg_solve'([H|T],SolveSol) :-
	bd_findall(HSol,neg_solve_positive(H,T,HSol),ListOfHSol),
	(member(success,ListOfHSol)
	  -> (SolveSol = success)
	  ;  (member(unknown,ListOfHSol)
		-> (SolveSol = unknown)
		;  (SolveSol = fail)
	     )
	).


neg_solve_positive(H,T,Sol) :-
	claus(_Nr,H,Body),
	append(Body,T,NewGoal),
	'neg_solve.always:neg_solve'(NewGoal,Sol).
