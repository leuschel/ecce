:- module(depth_bound,_).

/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,1996,1997 */
/* --------------------------------------------- */

/* TO GET THE LATEST VERSION OF THE ECCE SYSTEM GO TO:
	http://www.cs.kuleuven.ac.be/~michael */
/* BUG REPORTS, QUESTIONS AND COMMENTS CAN BE SENT TO:
	michael@cs.kuleuven.ac.be */

:- use_module(bimtools).

/* file: depth_bound.pro */

:- include( multi_meta ).

/* ============================================================ */
/*                        Depth-Bound Stuff                     */
/* ============================================================ */

pre_condition(depth_bound_ok(L)) :-
	term_is_of_type(L,unfold_history).
post_condition(depth_bound_ok(_L)).

depth_bound_ok(L) :-
	current_depth_bound(DB),
	((DB<1) -> (true)
		;  (list_shorter_than(L,DB))
	).

list_shorter_than(X,_Nr) :-
	var(X),!,print('### OPEN ENDED LIST in LIST_SHORTER_THAN'),nl,fail.
list_shorter_than([],_Nr).
list_shorter_than([_H|T],Nr) :-
	Nr>1,
	Nr1 is Nr - 1,
	list_shorter_than(T,Nr1).



/* ----------------------------- */
/* current_depth_bound/1 */
/* ----------------------------- */

:- dynamic current_depth_bound/1.
current_depth_bound(0).

/* ---------------------- */
/* set_depth_bound/0 */
/* ---------------------- */

set_depth_bound :-
	print('Enter a depth bound for unfolding (0 for no depth bound):'),nl,
	print('Current choice: '),
	current_depth_bound(Cur),
	print(Cur),nl,
	print('version =>'),
	read(NewVersionNr),
	set_new_depth_bound(NewVersionNr).

/* -------------------------- */
/* set_new_depth_bound/1 */
/* -------------------------- */

set_new_depth_bound(_NewNr) :-
	retract(current_depth_bound(_Cur)),
	fail.
set_new_depth_bound(NewNr) :-
	asserta(current_depth_bound(NewNr)).
