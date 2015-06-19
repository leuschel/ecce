:- module(static_dynamic_functors,[static_functor/2,defined_predicate/2,open_predicate/2,
set_treat_undefined_predicates_as_open/1,treat_undefined_as_fail/0,toggle_treatment_of_open_predicates/0,recalc_static_functors/0,reset_static_functors/0,calculate_static_functors/0,calculate_static_functors_for_query/1,add_static_functors/1,dynamic_term/1,add_defined_predicate/1,add_open_predicate/1,is_open_literal/1,static_conjunction/1,count_occurences/3,add_to_occurence_list/3,occurence_list_ok/1,update_sc_pred_count/1,convert_literal/2,convert_body/2
]).

/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,1996,1997 */
/* --------------------------------------------- */

/* TO GET THE LATEST VERSION OF THE ECCE SYSTEM GO TO:
	http://www.cs.kuleuven.ac.be/~michael */
/* BUG REPORTS, QUESTIONS AND COMMENTS CAN BE SENT TO:
	michael@cs.kuleuven.ac.be */

:- use_module(bimtools).
:- use_module(calc_chtree).

:- use_package( .('ecce_no_rt') ).


/* file: static_dynamic_functors.pro */

:- dynamic static_functor/2.
static_functor(_X,_Y).
:- dynamic defined_predicate/2.
defined_predicate(_X,_Y).
:- dynamic open_predicate/2.
open_predicate(_X,_Y).
:- dynamic sc_pred_count/2.


:- dynamic treat_undefined_predicates_as_open/1.
treat_undefined_predicates_as_open(yes).

set_treat_undefined_predicates_as_open(NewVal) :-
	retractall(treat_undefined_predicates_as_open(_)),
	assert(treat_undefined_predicates_as_open(NewVal)).

treat_undefined_as_fail :- set_treat_undefined_predicates_as_open(no).

toggle_treatment_of_open_predicates :-
	retract(treat_undefined_predicates_as_open(Val)),!,
	((Val=yes)
	 -> (NewVal=no, print('Treating open predicates as FAILING!'),nl)
	 ;  (NewVal=yes,
	     print('Treating open predicates as DEFINED somewhere else'),nl)),
	assert(treat_undefined_predicates_as_open(NewVal)).
toggle_treatment_of_open_predicates :-
	assert(treat_undefined_predicates_as_open(yes)).


recalc_static_functors :-
	reset_static_functors,
	calculate_static_functors.

reset_static_functors :-
	retract(static_functor(_X,_Y)),fail.
reset_static_functors :-
	retract(defined_predicate(_X,_Y)),fail.
reset_static_functors :-
	retract(open_predicate(_X,_Y)),fail.
reset_static_functors :-
	retract(sc_pred_count(_X,_Y)),fail.
reset_static_functors.

calculate_static_functors :-
	assert(static_functor([],0)),
	assert(static_functor('.',2)),
	fail.
calculate_static_functors :-
	claus(_Nr,Head,Body),
	add_static_functors(Head),
	add_defined_predicate(Head),
	l_add_static_functors(Body),
	l_add_static_conjunctions(Body),
	fail.
calculate_static_functors :-
	treat_undefined_predicates_as_open(yes),
	claus(_Nr,_Head,Body),
	l_add_open_predicates(Body),
	fail.
calculate_static_functors.



calculate_static_functors_for_query(Query) :-
	l_add_static_functors(Query),
	l_add_open_predicates(Query),
	l_add_static_conjunctions(Query),
	fail.
calculate_static_functors_for_query(_Query).



l_add_static_functors([]).
l_add_static_functors([H|T]) :-
	add_static_functors(H),
	l_add_static_functors(T).

add_static_functors(X) :- var(X),!.
add_static_functors(X) :-
	functor(X,F,Arity),!,
	(static_functor(F,Arity)
	 -> true
	 ;  assert(static_functor(F,Arity)),debug_println(sf(F,Arity))
	),
	X =.. [F|Args],!,
	l_add_static_functors(Args).

dynamic_term(X) :- var(X),!,fail.
dynamic_term(X) :-
	functor(X,F,Arity),
	\+(static_functor(F,Arity)).

add_defined_predicate(X) :- var(X),!.
add_defined_predicate(X) :-
	nonvar(X),
	functor(X,F,Arity),!,
	(defined_predicate(F,Arity)
	 -> true
	 ;  (assert(defined_predicate(F,Arity)),
	     debug_print(dp(F,Arity)),debug_nl)
	).


l_add_open_predicates([]).
l_add_open_predicates([H|T]) :-
	add_open_predicate(H),
	l_add_open_predicates(T).

add_open_predicate(X) :-
	nonvar(X),
	extract_positive_atom_from_literal(X,A),
	functor(A,F,Arity),!,
	(defined_predicate(F,Arity)
	 -> true
	 ;  (assert(open_predicate(F,Arity)),
	     debug_print(open(F,Arity)),debug_nl)
	).
add_open_predicate(_X).

is_open_literal(X) :- var(X),!,fail.
is_open_literal(X) :-
	functor(X,F,Arity),
	open_predicate(F,Arity).


/* ======================= */
/* static conjunction part */
/* ======================= */

static_conjunction(SC) :-
	convert_body(SC,CB),
	count_occurences(CB,[],OccList),
	occurence_list_ok(OccList).


l_add_static_conjunctions(Body) :-
	convert_body(Body,CB),
	count_occurences(CB,[],OccList),
	update_sc_pred_count(OccList).



count_occurences([],OccList,OccList).
count_occurences([Pred|T],InList,Res) :-
	add_to_occurence_list(Pred,InList,IntList),
	count_occurences(T,IntList,Res).

add_to_occurence_list(Pred,[],[d(Pred,1)]).
add_to_occurence_list(Pred,[d(Pred,Count)|T],[d(Pred,C1)|T]) :-
	C1 is Count + 1.
add_to_occurence_list(Pred,[d(Pred2,Count)|T],[d(Pred2,Count)|Res]) :-
	Pred \= Pred2,
	add_to_occurence_list(Pred,T,Res).
	

occurence_list_ok([]).
occurence_list_ok([d(Pred,Count)|T]) :-
	sc_pred_count(Pred,Max),
	Count =< Max,
	occurence_list_ok(T).


update_sc_pred_count([]).
update_sc_pred_count([d(Pred,Count)|T]) :-
	(sc_pred_count(Pred,Max)
	-> (Count =< Max
	     -> true
	     ;  (retractall(sc_pred_count(Pred,_)),
		 assertz(sc_pred_count(Pred,Count)))
	   )
	;  (assertz(sc_pred_count(Pred,Count)))
	),
	update_sc_pred_count(T).



convert_literal(Lit,CL) :-
	is_built_in_literal(Lit),!,
	get_predicate(Lit,CL).
convert_literal(Lit,CL) :-
	is_negative_literal(Lit,_Atom),!,
	(extract_positive_atom_from_literal(Lit,PosAtom)
	 -> (get_predicate(PosAtom,PL), CL = not(PL) )  /* maybe extend */
	 ;  get_predicate(Lit,CL)
	).
convert_literal(Atom,CA) :-
	get_predicate(Atom,CA).

convert_body([],[]).
convert_body([H|T],[CH|CT]) :-
	convert_literal(H,CH),
	convert_body(T,CT).
