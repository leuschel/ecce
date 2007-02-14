:- module('neg_solve.any',['neg_solve.any:neg_solve'/2]).

:- use_package( .('../ecce_no_rt2') ).


%:- dynamic neg_solve/2.

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module(library(aggregates)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../static_dynamic_functors').


/* supposes that evaluation of ground negated literals will terminate */

'neg_solve.any:neg_solve'(Goal,fail) :-
	dead(Goal),!.
'neg_solve.any:neg_solve'(Goal,SolveSol) :-
	ground(Goal),!,
	'neg_solve.any:neg_solve2'(Goal,SolveSol).
'neg_solve.any:neg_solve'(Goal,SolveSol) :-
	copy(Goal,CGoal),
	neg_solve_db(Goal,SolveSol,5,Goal,CGoal).
'neg_solve.any:neg_solve'(Goal,unknown).

neg_solve_db([],success,DB,Orig,OrigC) :-
	variant_of(Orig,OrigC),!.
neg_solve_db(Goal,fail,DB,Orig,OrigC) :-
	dead(Goal),!.
neg_solve_db([H|T],SolveSol,DB,Orig,OrigC) :-
	not(is_negative_literal(H,Atom)),
	not(is_built_in_literal(H)),
	not(is_open_literal(H)),
	DB > 0,
	DB1 is DB - 1,
	findall(HSol,
		neg_solve_positive_db(H,T,HSol,DB1,Orig,OrigC),ListOfHSol),
	(member(success,ListOfHSol)
	  -> (SolveSol = success)
	  ;  (member(unknown,ListOfHSol)
		-> (SolveSol = unknown)
		;  (SolveSol = fail)
	     )
	).
/* MISSING: HANDLE NEGATIONS  */
neg_solve_db([H|T],SolveSol,DB,Orig,OrigC) :-
	is_built_in_literal(H),!,
	(is_callable_built_in_literal(H)
	 -> (call_built_in(H)
		-> (neg_solve_db(T,SolveSol,DB,Orig,OrigC))
		;  (SolveSol = fail)
	    )
	 ;  (SolveSol = unknown)
	).

neg_solve_db(Goal,unknown,DB,Orig,OrigC).


neg_solve_positive_db(H,T,Sol,DB,Orig,OrigC) :-
	claus(Nr,H,Body),
	append(Body,T,NewGoal),
	neg_solve_db(NewGoal,Sol,DB,Orig,OrigC).


'neg_solve.any:neg_solve2'([],success) :- !.
'neg_solve.any:neg_solve2'(Goal,fail) :-
	dead(Goal),!.
'neg_solve.any:neg_solve2'([H|T],SolveSol) :-
	is_negative_literal(H,Atom),!,
	'neg_solve.any:neg_solve'([Atom],AtomSol),
	((AtomSol=fail)
	 -> (SolveSol = success)
	 ;  ((AtomSol=success)
	      -> (SolveSol = fail)
	      ;  (SolveSol = unknown)
	    )
	).
'neg_solve.any:neg_solve2'([H|T],SolveSol) :-
	is_built_in_literal(H),!,
	(is_callable_built_in_literal(H)
	 -> (call_built_in(H)
		-> ('neg_solve.any:neg_solve2'(T,SolveSol))
		;  (SolveSol = fail)
	    )
	 ;  (SolveSol = unknown)
	).
'neg_solve.any:neg_solve2'([H|T],SolveSol) :-
	findall(HSol,'neg_solve.any:neg_solve_positive'(H,T,HSol),ListOfHSol),
	(member(success,ListOfHSol)
	  -> (SolveSol = success)
	  ;  (member(unknown,ListOfHSol)
		-> (SolveSol = unknown)
		;  (SolveSol = fail)
	     )
	).


'neg_solve.any:neg_solve_positive'(H,T,Sol) :-
	claus(Nr,H,Body),
	append(Body,T,NewGoal),
	'neg_solve.any:neg_solve2'(NewGoal,Sol).
