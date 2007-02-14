/* --------- */
/* UNIFY.PRO */
/* --------- */
/* File: '$BIMTOOLS_PATH/unify.pro'. */

/* A  Unification Algorithm to facilitate information
   propagation during partial evaluation */

/* Der Stein der Weisen fuer die "Ground Representation" ?? */

/* -------------------------------------- */
/* Annotations for partial evaluator PECT */
/* -------------------------------------- */

:- dynamic open/1.
:- dynamic unfold/1.
:- dynamic dont_unfold/1.

open(term(numbervars,[any,any,any])).
open(term(unnumbervars,[any,any])).

/*
open(term(unify,[any,any,any,any])).
open(term(unify,[any,any,any,any,any,any])).
*/
unfold(term(unify,[nonvar,nonvar,any,any])).
unfold(term(unify,[nonvar,nonvar,any,any,any,any])).
unfold(term(unify_test,[nonvar,nonvar])).
unfold(term(unify_rename_test,[nonvar,nonvar])).

unfold(term(instance_of,[nonvar,nonvar])).

/* ----------------------------- */
/* pre/post conditions for unify */
/* ----------------------------- */

ecce_type(grterm,term(struct,[functor,list(grterm)])).
ecce_type(grterm,term('$VAR',[varindex])).
ecce_type(functor,ground).
ecce_type(varindex,ground).

ecce_type(unify_answer,term(success,[])).
ecce_type(unify_answer,term(fail,[])).

/* ----------- */
/* unification */
/* ----------- */

pre_condition(unify(T1,T2,E,EAfter)) :-
	term_is_of_type(T1,grterm),
	term_is_of_type(T2,grterm),
	term_is_of_type(E,grterm).
post_condition(unify(T1,T2,E,EAfter)) :-
	term_is_of_type(EAfter,grterm).

pre_condition(unify(T1,T2,E1,EAfter1,E2,EAfter2)) :-
	term_is_of_type(T1,grterm),
	term_is_of_type(T2,grterm),
	term_is_of_type(E1,grterm),
	term_is_of_type(E2,grterm).
post_condition(unify(T1,T2,E1,EAfter1,E2,EAfter2)) :-
	term_is_of_type(EAfter1,grterm),
	term_is_of_type(EAfter2,grterm).


unify(Term1,Term2,Environment,EnvAfterSub) :-
	unnumbervars(group(Term1,Term2,Environment),
			group(T12,T12,EnvAfterSub) ),
	numbervars(EnvAfterSub,1,_VarIndex).


unify(Term1,Term2,Env1,EnvAfterSub1,Env2,EnvAfterSub2) :-
	unnumbervars(group(Term1,Env1),
			group(T12,EnvAfterSub1) ),
	unnumbervars(group(Term2,Env2),
			group(T12,EnvAfterSub2) ),
	numbervars(gr(EnvAfterSub1,EnvAfterSub2),1,_VarIndex).



unify_test(Term1,Term2) :-
	unnumbervars(group(Term1,Term2),
			 group(T12,T12) ).


unify_rename_test(Term1,Term2) :-
	unnumbervars(Term1,T12),
	unnumbervars(Term2,T12).



/* ================================================================= */

/* ------------- */
/* instance_of/2 */
/* ------------- */
/* tests whether the first argument is an instance of the second one */

pre_condition(instance_of(X,Y)) :-
	term_is_of_type(X,grterm),
	term_is_of_type(Y,grterm).
post_condition(instance_of(X,Y)).


instance_of(X,Y) :-
	unnumbervars(Y,X).



