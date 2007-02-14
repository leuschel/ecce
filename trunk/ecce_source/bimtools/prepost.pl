/* ----------------------------------------------------- */
/* CHECKING PRE- AND POST-CONDITIONS FOR PREDICATE CALLS */
/* ----------------------------------------------------- */
/* File: prepost.nocheck.pro */

:- dynamic prepost_call/1, prepost_mnf_call/1, mnf_call/1, pp_mnf/1, pp_cll/1.
:- dynamic verify_pre/1,verify_post/1.

/* ===================================================== */


pp_mnf(X) :- call(X).
pp_cll(X) :- call(X).

/* ===================================================== */

prepost_call(X) :- call(X).
prepost_mnf_call(X) :- call(X).
mnf_call(X) :- call(X).

verify_pre(_Call).

verify_post(_Call).

/* ===================================================== */
