:- module(self_check,[must_fail/1,must_succeed/1,perform_self_check/0]).
/* file: self-check.pro */

:- use_module(bimtools).

:- use_module(library(lists)).

:- use_module(global_tree).
:- use_module(calc_chtree).
:- use_module(homeomorphic).
%:- use_module(constraints).


:- use_package( .('ecce_no_rt') ).

:- multifile self_check/1.
:- dynamic self_check/1.

:- meta_predicate must_fail(    goal ).
:- meta_predicate must_succeed( goal ).

must_fail(X) :-
	copy(X,Y),
	X,
	print('### Self-Check Failed !!!'),nl,
	print('### The call: '), nl,
	print(Y),nl,
	print('### should have failed but succeeded with:'),nl,
	print(X),nl.
must_fail(_X).


must_succeed(X) :-
	%display(X),nl,
	not(X),
	print('### Self-Check Failed !!!'),nl,
	print('### The call: '), nl,
	print(X),nl,
	print('### should have suceeded but failed !'),nl.
must_succeed(_X).


perform_self_check :-
   print('Performing Self-Check'),nl,
   self_check(X),
   debug_print(X),print('.'),debug_nl,
   call(X),
   fail.
perform_self_check :- nl,print('Self-Check Finished'),nl.
