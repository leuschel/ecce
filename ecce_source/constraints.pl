:- module(constraints,_).

:- use_package( .(ecce_no_rt) ).

/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,1996,1997 */
/* --------------------------------------------- */

/* TO GET THE LATEST VERSION OF THE ECCE SYSTEM GO TO:
	http://www.cs.kuleuven.ac.be/~michael */
/* BUG REPORTS, QUESTIONS AND COMMENTS CAN BE SENT TO:
	michael@cs.kuleuven.ac.be */

/* file: constraints.pro */

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).


:- use_module(library(lists)).

/*
:- ensure_consulted('$BIMTOOLS_PATH/typechecker.pro').
:- ensure_consulted('$BIMTOOLS_PATH/prepost.pro').
:- ensure_consulted('$BIMTOOLS_PATH/bd_findall.pro').
:- ensure_consulted('$BIMTOOLS_PATH/StdLists.pro').
*/
:- use_module(bimtools).
:- use_module(calc_chtree).

%:- use_module(self_check).

/* ===================================================== */

:- include( multi_meta ).

/* ===================================================== */

/* ++++++++++++++++ */
/* TYPE DEFINITIONS */
/* ++++++++++++++++ */
ecce_type(constraint,list(elementary_constraint)).
ecce_type(constraint,term(fail,[])).
ecce_type(elementary_constraint,term(ecce_type,[nonvar,any])).
ecce_type(simplified_constraint,list(elementary_simplified_constraint)).
ecce_type(elementary_simplified_constraint,term(ecce_type,[nonvar,var])).


/* TYPE CHECKING PART */

regular_type(list(_X),[],[]).
regular_type(list(X),[H|T],[ecce_type(X,H), ecce_type(list(X),T)]).
regular_type(cst(X),X,[]).
regular_type(any,_,[]).
regular_type(rec(Base,_Func),Base,[]).
regular_type(rec(Base,Func),X,[ecce_type(rec(Base,Func),Arg)]) :- X=..[Func,Arg].
regular_type(rec2(Base,_Func),Base,[]).
regular_type(rec2(Base,Func),X,
    [ecce_type(rec2(Base,Func),Arg1),ecce_type(rec2(Base,Func),Arg2)]) :-
             X=..[Func,Arg1,Arg2].

is_of_regular_type(Type,X) :- regular_type(Type,X,Cond), check_condition(Cond).

check_condition([]).
check_condition([ecce_type(Type,X)|T]) :- 
  is_of_regular_type(Type,X),
  check_condition(T).


/* CONSTRAINT MANIPULATIONS */

/* ------------------ */
/* constraint_union/3 */
/* ------------------ */

pre_condition(constraint_union(C,C2,_)) :-
	term_is_of_type(C,constraint),
	term_is_of_type(C2,constraint).
post_condition(constraint_union(_C,_C2,R)) :-
	term_is_of_type(R,constraint).

constraint_union(fail,_,fail) :- !.
constraint_union(_,fail,fail) :- !.
constraint_union(C1,C2,R) :-
  pp_cll(append(C1,C2,R)).  /* to be improved */



/* ---------------- */
/* constraint_msg/6 */
/* ---------------- */

pre_condition(constraint_msg(G,C,G2,C2,_,_)) :-
	term_is_of_type(C,constraint),
	term_is_of_type(C2,constraint),
	term_is_of_type(G,goal),
	term_is_of_type(G2,goal).
post_condition(constraint_msg(_G,_C,_G2,_C2,R,RC)) :-
	term_is_of_type(RC,constraint),
	term_is_of_type(R,goal).

constraint_msg(G1,C1,G2,C2,GMSG,CMSG) :- 
   msg(G1,G2,GMSG),
   constraint_union(C1,C2,CMSG).
 /* to be improved !! only works for C1/2 =  [] or fail */


/* ------------------------ */
/* constraint_instance_of/4 */
/* ------------------------ */

pre_condition(constraint_instance_of(G,C,G2,C2)) :-
	term_is_of_type(C,constraint),
	term_is_of_type(C2,constraint),
	term_is_of_type(G,goal),
	term_is_of_type(G2,goal).
post_condition(constraint_instance_of(_G,_C,_G2,_C2)).


self_check(must_succeed(
   (pp_mnf(constraint_instance_of([p(a)],[],[p(a)],[]))))).
self_check(must_succeed(
   (pp_mnf(constraint_instance_of([p(a)],[],[p(_X)],[]))))).
self_check(must_succeed(
   (pp_mnf(constraint_instance_of([p(a)],[],[p(X)],[ecce_type(cst(a),X)]))))).
self_check(must_succeed(
   (pp_mnf(constraint_instance_of([p([a])],[],[p(X)],[ecce_type(list(cst(a)),X)]))))).
self_check(must_fail(
   (pp_cll(constraint_instance_of([p(_X)],[],[p(a)],[]))))).
self_check(must_fail(
   (pp_cll(constraint_instance_of([p(a)],[],[p(X)],[ecce_type(cst(b),X)]))))).
self_check(must_fail(
   (pp_cll(constraint_instance_of([p(a)],[],[p(X)],[ecce_type(list(cst(a)),X)]))))).
self_check(must_fail(
   (pp_cll(constraint_instance_of([p([a])],[],[p(X)],[ecce_type(list(cst(b)),X)]))))).

constraint_instance_of(Goal,Constraint,MoreGeneralGoal,MC) :-
   instance_of(Goal,MoreGeneralGoal),
   ((Constraint=[])
       -> ( copy(Goal,CGoal),
            copy((MoreGeneralGoal,MC),(G2,C2)),
            G2 = CGoal,
            pp_cll(simplify_constraint(C2,_)) )
       ;  (print(' ? ')) 
    ).  /* expand !!! */

/* --------------------- */
/* simplify_constraint/2 */
/* --------------------- */

pre_condition(simplify_constraint(C,_SC)) :-
	term_is_of_type(C,constraint).
post_condition(simplify_constraint(_C,SC)) :-
	term_is_of_type(SC,constraint).

self_check(must_succeed(
   (pp_mnf(simplify_constraint([ecce_type(list(any),X)],L)), 
    L == [ecce_type(list(any),X)]))).
self_check(must_succeed(
   (pp_mnf(simplify_constraint([ecce_type(rec(a,f),f(f(a)))],L)), 
    L == []))).
self_check(must_succeed(
   (pp_mnf(simplify_constraint([ecce_type(rec2(a,f),f(a,f(X,f(a,a))))],L)), 
    L == [ecce_type(rec2(a,f),X)]))).
self_check(must_succeed(
   (pp_mnf(simplify_constraint([ecce_type(list(any),[_H|T])],L)), 
    L == [ecce_type(list(any),T)]))).
self_check(must_succeed(
   (pp_mnf(simplify_constraint([ecce_type(cst(2),2)],L)),
    L == []))).
self_check(must_succeed(
   (pp_mnf(simplify_constraint(
    [ecce_type(list(cst(2)),[2,2,2|T]), ecce_type(list(any),X)],L)),
    L == [ecce_type(list(cst(2)),T),ecce_type(list(any),X)]))).
self_check(must_fail(
   pp_cll(simplify_constraint(
    [ecce_type(list(cst(2)),[2,2,4|_T]), ecce_type(list(any),_X)],_L)))).

simplify_constraint([],[]).
simplify_constraint([ecce_type(Type,X)|T],SC) :-
  ((nonvar(X) ; (Type=any)) ->
     (regular_type(Type,X,Cond),
      simplify_constraint(Cond,SCond),
      append(SCond,ST,SC)
     )
   ; (SC = [ecce_type(Type,X)|ST])
  ),
  simplify_constraint(T,ST).

/* ------------- */
/* satisfiable/1 */
/* ------------- */

satisfiable(Constraint) :-
  pp_cll(simplify_constraint(Constraint,_)).

/* --------------------- */
/*  project_constraint/3 */
/* --------------------- */

pre_condition(project_constraint(C,_G,_SC)) :-
	term_is_of_type(C,constraint).
post_condition(project_constraint(_C,_G,SC)) :-
	term_is_of_type(SC,constraint).

self_check(must_succeed(
   (pp_mnf(project_constraint([ecce_type(list(any),X)],X,L)),
    L == [ecce_type(list(any),X)]))).
self_check(must_succeed(
   (pp_mnf(project_constraint([ecce_type(list(any),_Z)],f(X,X),L)),
    L == []))).
self_check(must_succeed(
   (pp_mnf(project_constraint([],f(_X),L)),
    L == []))).
self_check(must_succeed(
   (pp_mnf(project_constraint(
          [ecce_type(list(any),_Z),ecce_type(list(any),X),ecce_type(cst(2),_V),
           ecce_type(list(cst(1)),[X])],f(X,_Y),L)),
    L == [ecce_type(list(any),X),ecce_type(list(cst(1)),[X])]))).

project_constraint(fail,_H,fail).
project_constraint([],_H,[]).
project_constraint([C|Cs],H,Constraint) :-
  (sharing(C,H) -> (Constraint=[C|PCs]) ; (Constraint = PCs)),
  project_constraint(Cs,H,PCs).


/* --------------------- */
/*  project_constraint/4 */
/* --------------------- */

pre_condition(project_simplified_constraint(C,_G,_SC,_Rem)) :-
	term_is_of_type(C,simplified_constraint).
post_condition(project_simplified_constraint(_C,_G,SC,Rem)) :-
	term_is_of_type(SC,constraint),
	term_is_of_type(Rem,constraint).

self_check(must_succeed(
   (pp_mnf(project_simplified_constraint([ecce_type(list(any),X)],X,L,R)),
    L == [ecce_type(list(any),X)], R==[]))).
self_check(must_succeed(
   (pp_mnf(project_simplified_constraint([ecce_type(list(any),_Z)],f(X,X),L,R)),
    L == [], R==[ecce_type(list(any),_Z)] ))).
self_check(must_succeed(
   (pp_mnf(project_simplified_constraint([],f(_X),L,R)),
    L == [], R==[]))).
self_check(must_succeed(
   (pp_mnf(project_simplified_constraint(
          [ecce_type(list(any),_Z),ecce_type(list(any),X),ecce_type(cst(2),_V),
           ecce_type(list(cst(1)),X)],f(X,_Y),L,R)),
    L == [ecce_type(list(any),X),ecce_type(list(cst(1)),X)],
    R == [ecce_type(list(any),_Z), ecce_type(cst(2),_V)] ))).

project_simplified_constraint([],_H,[],[]).
project_simplified_constraint([C|Cs],H,Constraint,Rem) :-
  (sharing(C,H) ->
    (Constraint=[C|PCs],Rem=RemCs)
  ; (Constraint = PCs, Rem=[C|RemCs])),
  project_simplified_constraint(Cs,H,PCs,RemCs).

