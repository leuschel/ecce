:- module('neg_solve.ground',['neg_solve.ground:neg_solve'/2]).

%:- dynamic neg_solve/2.

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module(library(aggregates)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').

/* supposes that evaluation of ground negated literals will terminate */


'neg_solve.ground:neg_solve'(Goal,fail) :-
	dead(Goal),!.
'neg_solve.ground:neg_solve'(Goal,SolveSol) :-
	ground(Goal),!,
	'neg_solve.ground:neg_solve2'(Goal,SolveSol).
'neg_solve.ground:neg_solve'(_Goal,unknown).


'neg_solve.ground:neg_solve2'([],success) :- !.
'neg_solve.ground:neg_solve2'(Goal,fail) :-
	dead(Goal),!.
'neg_solve.ground:neg_solve2'([H|_T],SolveSol) :-
	is_negative_literal(H,Atom),!,
	'neg_solve.ground:neg_solve'([Atom],AtomSol),
	((AtomSol=fail)
	 -> (SolveSol = success)
	 ;  ((AtomSol=success)
	      -> (SolveSol = fail)
	      ;  (SolveSol = unknown)
	    )
	).
'neg_solve.ground:neg_solve2'([H|T],SolveSol) :-
	is_built_in_literal(H),!,
	(is_callable_built_in_literal(H)
	 -> (call_built_in(H)
		-> ('neg_solve.ground:neg_solve2'(T,SolveSol))
		;  (SolveSol = fail)
	    )
	 ;  (SolveSol = unknown)
	).
'neg_solve.ground:neg_solve2'([H|T],SolveSol) :-
	findall(HSol,'neg_solve.ground:neg_solve_positive'(H,T,HSol),ListOfHSol),
	(member(success,ListOfHSol)
	  -> (SolveSol = success)
	  ;  (member(unknown,ListOfHSol)
		-> (SolveSol = unknown)
		;  (SolveSol = fail)
	     )
	).


'neg_solve.ground:neg_solve_positive'(H,T,Sol) :-
	claus(_Nr,H,Body),
	append(Body,T,NewGoal),
	'neg_solve.ground:neg_solve2'(NewGoal,Sol).
