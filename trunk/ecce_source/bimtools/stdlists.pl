
/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,1996,1997 */
/* --------------------------------------------- */


/* ------------ */
/* StdLists.pro */
/* ------------ */


/* Standard List Manipulation tools */

% :- use_module(library(lists)).

% FOR CIAO!
%:-include( '../multi_meta' ).

% FOR SICSTUS!
%:-include( 'multi_meta' ).


/* ------ */
/* append */
/*------- */

self_check(must_succeed(append([1],[2],_R))).
self_check(must_succeed(append([1,2],[3],[1,2,3]))).
self_check(must_fail(append([_X],[3],[1,2,3]))).

pre_condition(append(X,_Y,_Z)) :-
	term_is_of_type(X,list(any),no),!.
pre_condition(append(_X,_Y,Z)) :-
	term_is_of_type(Z,list(any)).

post_condition(append(_X,_Y,_Z)).

/* already defined via :- use_module(library(list)). 
append([],L,L).
append([H|T],M,[H|T2]) :-
	append(T,M,T2).
*/

/* ---------- */
/*   member   */
/* ---------- */

self_check(must_succeed(member(1,[1,2,3]))).
self_check(must_succeed(member(2,[1,2,3]))).
self_check(must_succeed(member(3,[1,2,3]))).
self_check(must_fail(member(4,[1,2,3,3]))).

pre_condition(member(_X,List)) :-
	term_is_of_type(List,list(any)).

post_condition(member(_X,_List)).

/* already defined via :- use_module(library(list)). 
member(X,[X|_T]).
member(X,[_Y|T]) :-
	member(X,T).
*/

/* ---------- */
/*   member   */
/* ---------- */

self_check(must_succeed(exact_member(1,[1,2,3]))).
self_check(must_succeed(exact_member(X,[1,X,3]))).
self_check(must_succeed(exact_member(X,[1,2,X]))).
self_check(must_fail(exact_member(_X,[1,_Y,3,3]))).

pre_condition(exact_member(_X,List)) :-
	term_is_of_type(List,list(any)).

post_condition(exact_member(_X,_List)).
 
exact_member(X,[Y|T]) :- 
 ((X==Y) -> true ; exact_member(X,T)).


/* ------------- */
/*   member_nr   */
/* ------------- */

/* just like member but also returns position of the member in the list */


self_check(must_succeed(member_nr(2,[1,2,3],2))).
self_check(must_succeed(member_nr(3,[1,2,3],3))).
self_check(must_fail(member_nr(_X,[1,2,3],4))).
self_check(must_fail(member_nr(4,[1,2,3],_X))).
self_check(must_fail(member_nr(0,[1,2,3],_X))).

pre_condition(member_nr(_X,List,_Nr)) :-
	term_is_of_type(List,list(any)).

post_condition(member_nr(_X,_List,Nr)) :-
	term_is_of_type(Nr,integer).

member_nr(X,[X|_T],1).
member_nr(X,[_Y|T],Nr) :-
	member_nr(X,T,TNr),
	Nr is TNr + 1.
/* one could use the pre-define nth */


/* ---------------- */
/* length(List,Len) */
/* ---------------- */

self_check(must_succeed(length([1,2,3],3))).
self_check(must_succeed(length([],0))).
self_check(must_fail(length([1,2,3],2))).
self_check(must_fail(length([],2))).

pre_condition(length(List,_Len)) :-
	term_is_of_type(List,list(any)).

post_condition(length(_List,Len)) :-
	term_is_of_type(Len,integer).


/* --------------------------------------- */
/* split_list(List,Pos,Left,ElAtPos,Right) */
/* --------------------------------------- */

/* splits list at a given position */

self_check(must_succeed(split_list([a,b,c],2,[a],b,[c]))).
self_check(must_succeed(split_list([a,b,c],1,[],a,[b,c]))).
self_check(must_succeed(split_list([a,b,c],3,[a,b],c,[]))).
self_check(must_fail(split_list([a,b,c],0,_,_,_))).
self_check(must_fail(split_list([a,b,c],4,_,_,_))).

pre_condition(split_list(List,Pos,_Left,_ElAtPos,_Right)) :-
	term_is_of_type(List,list(any)),
	term_is_of_type(Pos,integer).

post_condition(split_list(_List,_Pos,Left,_ElAtPos,Right)) :-
	term_is_of_type(Left,list(any)),
	term_is_of_type(Right,list(any)).

split_list([H|T],1,[],H,T) :- !.
split_list([H|T],Nr,[H|Left],ElAtNr,Right) :-
	Nrm1 is Nr - 1,
	split_list(T,Nrm1,Left,ElAtNr,Right).


/* ------------------------ */
/* reverse(List,Acc,Result) */
/* ------------------------ */

self_check(must_succeed(reverse([1,2,3],[3,2,1]))).
self_check(must_succeed(reverse([],[]))).
self_check(must_succeed(reverse([1],[1]))).
self_check(must_fail(reverse([1,2],[1,2]))).
self_check(must_fail(reverse([1,2],[2,1,0]))).
self_check(must_fail(reverse([1,2],[2]))).

pre_condition(reverse(List,_Result)) :-
	term_is_of_type(List,list(any)).
post_condition(reverse(_List,Result)) :-
	term_is_of_type(Result,list(any)).

/* already defined in library(lists)
reverse(L,RevL) :-
	reverse(L,[],RevL).
*/

/* ------------------------ */
/* reverse(List,Acc,Result) */
/* ------------------------ */

pre_condition(reverse(List,Acc,_Result)) :-
	term_is_of_type(List,list(any)),
	term_is_of_type(Acc,list(any)).
post_condition(reverse(_List,_Acc,Result)) :-
	term_is_of_type(Result,list(any)).




