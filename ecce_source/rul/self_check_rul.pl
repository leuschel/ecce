%###################################################################
%######## MODULE SELF CHECK: LOCAL COPY OF MICHAEL'S MODULE ########
%# Must not be used outside the RUL package on higher ECCE levels! #
%###################################################################

:- module(self_check_rul,
    [assert_pre/2, assert_post/2,pp_mnf/1, pp_cll/1,
     mnf/1, assert_must_succeed/1,assert_must_fail/1,
     must_fail/1, must_succeed/1, add_self_check/1,
     perform_self_check/0]).

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(gensym2,[gensym/2]).

/* Example use:
:- assert_pre(user:b_fd_type(_G,_L,_U),true).
:- assert_post(user:b_fd_type(G,L,U),(atomic(G),(integer(L),integer(U)))).
*/

do no exectue thissss

:-include( '../multi_meta' ).


assert_pre(X,Pre) :- 
    ((nonvar(X),nonvar(Pre))
       -> (retractall(pre_condition(X,_)),assert(pre_condition(X,Pre)))
       ;  (print('### illegal variable(s) in: '),
           print(assert_pre(X,Pre)),nl,fail)
    ).
assert_post(X,Post) :- 
    ((nonvar(X),nonvar(Post))
       -> (retractall(post_condition(X,_)),assert(post_condition(X,Post)))
       ;  (print('### illegal variable(s) in: '),
           print(assert_post(X,Post)),nl,fail)
    ).

/* ===================================================== */

:- dynamic prepost_mnf_flag/1.

:- dynamic prepost_no_error_so_far/0.

prepost_no_error_so_far.

/* ===================================================== */

pp_mnf(X) :- prepost_mnf_call(X).
pp_cll(X) :- prepost_call(X).
mnf(X) :- mnf_call(X).

/* ===================================================== */

prepost_call(X) :-
	/* user:debug_print(pre(X)),user:debug_nl,*/
	verify_pre(X),
	call(X),
        /* user:debug_print(post(X)),user:debug_nl,*/
	verify_post(X).

verify_pre(Call) :-
	(pre_condition(Call,Pre)
	->( (\+(call(Pre))
		-> (print('### PRE-CONDITION ERROR OCCURED'),nl,
	   	    print('### '),print(Call),nl,
                    prepost_user_interaction
		   )
		;  true
	     )
	  )
	; (print('### No PRE-CONDITION for'),nl,
	   print('### '),print(Call),nl,
           prepost_user_interaction )
	).

verify_post(Call) :-
	(post_condition(Call,Post)
	->( (\+(call(Post))
		-> (print('### POST-CONDITION ERROR OCCURED'),nl,
	   	    print('### '),print(Call),nl,
                    prepost_user_interaction
		   )
		;  true
	     )
	  )
	; (print('### No POST-CONDITION for'),nl,
	   print('### '),print(Call),nl,
           prepost_user_interaction )
	).

prepost_user_interaction :- prepost_no_error_so_far,
       print('### => Stop at next error (y/n) => '),
       read(Answer),
       ((Answer='y') -> true ; retract(prepost_no_error_so_far)).

/* ===================================================== */

prepost_mnf_call(X) :-
	gensym2:gensym(mnf_flag,Flag),
	prepost_mnf_call(X,Flag).

prepost_mnf_call(X,Flag) :- /* mnf = must not fail */
	verify_pre(X),
	call(X),
	asserta(prepost_mnf_flag(Flag)),
	verify_post(X).
prepost_mnf_call(X,Flag) :-
	\+(prepost_mnf_flag(Flag)),
	print('### WARNING CALL HAS FAILED !'),nl,
	print('### '),print(X),nl,
        prepost_user_interaction,
	fail.
prepost_mnf_call(_X,Flag) :-
	retract(prepost_mnf_flag(Flag)),fail.

mnf_call(X) :-
	gensym2:gensym(mnf_flag,Flag),
	mnf_call(X,Flag).
mnf_call(X,Flag) :- /* mnf = must not fail */
	call(X),
	asserta(prepost_mnf_flag(Flag)).
mnf_call(X,Flag) :-
	\+(prepost_mnf_flag(Flag)),
	print('### WARNING CALL HAS FAILED !'),nl,
	print('### '),print(X),nl,
        prepost_user_interaction,
	fail.
mnf_call(_X,Flag) :-
	retract(prepost_mnf_flag(Flag)),fail.

/* ===================================================== */

:- dynamic self_check/1.

add_self_check(X) :-
    (nonvar(X)
       -> assert(self_check(X))
       ;  (print('### trying to assert variable as self_check: '),print(X),nl,fail)
    ).

assert_must_succeed(X) :-
    add_self_check(must_succeed(X)).
assert_must_fail(X) :-
    add_self_check(must_fail(X)).

must_fail(X) :-
	copy_term(X,Y),
	X,!,
	print('### Self-Check Failed !!!'),nl,
	print('### The call: '), nl,
	print(Y),nl,
	print('### should have failed but succeeded with:'),nl,
	print(X),nl.
must_fail(_X).


must_succeed(X) :-
	\+(X),!,
	print('### Self-Check Failed !!!'),nl,
	print('### The call: '), nl,
	print(X),nl,
	print('### should have suceeded but failed !'),nl.
must_succeed(_X).


perform_self_check :-
   print('Performing Self-Check'),nl,
   self_check(X),
   print('.'),
   call(X),
   fail.
perform_self_check :- nl,print('Self-Check Finished'),nl.

%#############################################################









