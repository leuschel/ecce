
/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,1996,1997 */
/* --------------------------------------------- */


/* ----------------------------------------------------- */
/* CHECKING PRE- AND POST-CONDITIONS FOR PREDICATE CALLS */
/* ----------------------------------------------------- */
/* File: prepost.pl */


/* ===================================================== */
:- dynamic prepost_call/1, prepost_mnf_call/1, mnf_call/1, pp_mnf/1, pp_cll/1.
:- dynamic verify_pre/1,verify_post/1.

:- meta_predicate pp_mnf(goal).
:- meta_predicate pp_cll(goal).

:- meta_predicate prepost_mnf_call(goal).
:- meta_predicate prepost_call(goal).

/*
:- ensure_consulted('$BIMTOOLS_PATH/typechecker.pro').
:- ensure_consulted('$BIMTOOLS_PATH/gensym.pro').
*/

/* ===================================================== */

:- dynamic prepost_mnf_flag/1.

:- dynamic prepost_no_error_so_far/0.

prepost_no_error_so_far.

/* ===================================================== */


/* example of use:

when defining a predicate to sort lists for instance
	(we also use the typechecker primitives):

pre_condition(sort(List,Result)) :-
	term_is_of_type(List,list(integer)),
	term_is_of_type(Result,var). -> output variable 

post_condition(sort(List,Result)) :-
	term_is_of_type(Result,list(integer)),
	sorted(Result).

sort([],[]).
sort([H|T],Res) :- ....prepost_call(sort(T,TRes)),....

or                 ....prepost_mnf_call(sort(T,TRes)),....

	mnf = must not fail

*/

/* ===================================================== */


pp_mnf(X) :- prepost_mnf_call(X).
pp_cll(X) :- prepost_call(X).

/* ===================================================== */

prepost_call(X) :-
 	/*print(pre(X)),nl,*/
 	verify_pre(X),
% %	module_concat( M , X , MX ),
% %	term_to_meta( MX , MXT ),
 	call(X),
 	/*print(post(X)),nl,*/
 	verify_post(X).


verify_pre(Call) :-
	reset_type_error,
	(pre_condition(Call)
	->( (type_error_occurred
		-> (print('### PRE-CONDITION TYPE ERROR OCCURED'),nl,
	   	    print('### '),safe_print_term(Call),nl,
                    prepost_user_interaction
		   )
		;  true
	     )
	  )
	; (print('### PRE-CONDITION Failed'),nl,
	   print('### '),safe_print_term(Call),nl,
           prepost_user_interaction )
	).

verify_post(Call) :-
	reset_type_error,
	(post_condition(Call)
	->( (type_error_occurred
		-> (print('### POST-CONDITION TYPE ERROR OCCURED'),nl,
	   	    print('### '),safe_print_term(Call),nl,
                    prepost_user_interaction
		   )
		;  true
	     )
	  )
	; (print('### POST-CONDITION Failed'),nl,
	   print('### '),safe_print_term(Call),nl,
           prepost_user_interaction )
	).

prepost_user_interaction :- prepost_no_error_so_far,
       print('### => Stop at next error (y/n) => '),
       read(Answer),
       ((Answer='y') -> true ; retract(prepost_no_error_so_far)).

/* ===================================================== */


prepost_mnf_call(X) :-
	gensym(mnf_flag,Flag),
	prepost_mnf_call2(X,Flag).

prepost_mnf_call2(X,Flag) :- /* mnf = must not fail */
	verify_pre(X),
% 	module_concat( M , X , MX ),
% 	term_to_meta( MX , MXT ),
	call(X),
	asserta(prepost_mnf_flag(Flag)),
	verify_post(X).
prepost_mnf_call2(X,Flag) :-
	\+(prepost_mnf_flag(Flag)),
	print('### WARNING CALL HAS FAILED !'),nl,
	print('### '),safe_print_term(X),nl,
        prepost_user_interaction,
	fail.
prepost_mnf_call2(_X,Flag) :-
	retract(prepost_mnf_flag(Flag)),fail.

	

mnf_call(X) :-
	gensym(mnf_flag,Flag),
	mnf_call(X,Flag).
mnf_call(X,Flag) :- /* mnf = must not fail */
	call(X),
	asserta(prepost_mnf_flag(Flag)).
mnf_call(X,Flag) :-
	\+(prepost_mnf_flag(Flag)),
	print('### WARNING CALL HAS FAILED !'),nl,
	print('### '),safe_print_term(X),nl,
        prepost_user_interaction,
	fail.
mnf_call(_X,Flag) :-
	retract(prepost_mnf_flag(Flag)),fail.
  

/* ===================================================== */
