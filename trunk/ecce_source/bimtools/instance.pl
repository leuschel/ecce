
/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,1996,1997 */
/* --------------------------------------------- */

% :- use_module(library(terms)).

/* ---------- */
/* VARIANT_OF */
/* ---------- */

self_check(must_fail(variant_of(p(_X),p(a)))).
self_check(must_fail(variant_of(p(a),p(_X)))).
self_check(must_fail(variant_of(q(X,_Y),q(X,X)))).
self_check(must_succeed(variant_of(q(X,Y),q(Y,X)))).
self_check(must_succeed(variant_of(q(X,Y),q(X,Y)))).

/* test whether something is variant of something else */


/* BIM Version:
variant_of(Goal,UIGoal) :- copy(Goal,CGoal),
			not(not(( numbervars(CGoal,0,N),
				  numbervars(UIGoal,0,N),
				  CGoal = UIGoal))).
*/

/* ===================================================== */

/* ----------- */
/* INSTANCE_OF */
/* ----------- */

/* tests whether Goal is an instance (covered) by UIGoal */
/* copy is made below, so that ?-covered(p(a,X),p(X,Y)). succeeds */

self_check(must_fail(instance_of(q(X),q(f(X))))).
self_check(must_fail(instance_of(q(X,_Y),q(X,X)))).
self_check(must_fail(instance_of(q(_Y,a),q(X,X)))).
self_check(must_fail(instance_of(q(X,X),q(_Y,a)))).
self_check(must_fail(instance_of(r,q(X,X)))).
self_check(must_fail(instance_of(p([s],_X),s))).
self_check(must_succeed(instance_of(q(X),q(X)))).
self_check(must_succeed(instance_of(q(f(X)),q(X)))).
self_check(must_succeed(instance_of(q(X,Y),q(Y,X)))).
self_check(must_succeed(instance_of(q(X,Y),q(Y,X)))).

/* BIM Version:
instance_of(Goal,UIGoal) :- copy(Goal,CGoal),
			not(not((numbervars(CGoal,0,_N),
				 CGoal = UIGoal))).
*/

/* ===================================================== */

/* ------------------ */
/* STRICT_INSTANCE_OF */
/* ------------------ */

self_check(must_fail(strict_instance_of(q(_Z,_V),q(_X,_Y)))).
self_check(must_fail(strict_instance_of(q(Z,V),q(Z,V)))).
self_check(must_fail(strict_instance_of(q(X,X),q(_Y,a)))).
self_check(must_fail(strict_instance_of(q(_T,a),q(X,X)))).
self_check(must_fail(strict_instance_of(r,q(X,X)))).
self_check(must_fail(strict_instance_of(p([s],_X),s))).
self_check(must_succeed(strict_instance_of(q(f(X)),q(X)))).
self_check(must_succeed(strict_instance_of(q(X,X),q(X,_Y)))).


/* BIM Version:
strict_instance_of(Goal1,Goal2) :-
	instance_of(Goal1,Goal2),
	not(instance_of(Goal2,Goal1)).
*/

