/**************************************************************************/
/*                 STATIC - CONJUNCTION - ANALYSIS                        */
/*                                                                        */
/*   File  : static_conj.pro                                              */
/*   Author: Jesper Jorgensen                                             */
/*   Date  : 18/4/96                                                      */
/*   Desc  : Finds maximum of predicate that may occur in conjunctions    */
/*           during conjunctive partial deduction. This will prevent      */
/*           an exponential blowup in the number of conjunction that can  */
/*           occur during conjunctive partial deduction.                  */
/**************************************************************************/
/* require definitions of: append and member */
/* :- ensure_consulted('$BIMTOOLS_PATH/StdLists.pro'). */

:- module(static_conjunction,_).

:- dynamic sc_clause/2.
:- dynamic memo/1.
:- dynamic sc_pred_count/2.


static_conjunction(SC) :-
	convert_body(SC,CB),
	count_occurences(CB,[],OccList),
	occurence_list_ok(OccList).

count_occurences([],OccList,OccList).
count_occurences([Pred|T],InList,Res) :-
	add_to_occurence_list(Pred,InList,IntList),
	count_occurences(T,IntList,Res).

add_to_occurence_list(Pred,[],[d(Pred,1)]).
add_to_occurence_list(Pred,[d(Pred,Count)|T],[d(Pred,C1)|T]) :-
	C1 is Count + 1.
add_to_occurence_list(Pred,[d(Pred2,Count)|T],[d(Pred2,Count)|Res]) :-
	Pred \== Pred2,
	add_to_occurence_list(Pred,T,Res).
	

occurence_list_ok([]).
occurence_list_ok([d(Pred,Count)|T]) :-
	sc_pred_count(Pred,Max),
	Count =< Max,
	occurence_list_ok(T).

go_sca(PEGoal) :- 
	generate_my_program,
	convert_body(PEGoal,ConvGoal),
	debug_print('Start sca...'),
        initialize,
	/* sca(D), */
	sca_goal(ConvGoal,D),
	debug_print('Done!'), debug_nl,
	debug_print(D), debug_nl,
	assert_descriptions(D).

assert_descriptions([]).
assert_descriptions([d(Pred,Count)|T]) :-
	assert(sc_pred_count(Pred,Count)),
	assert_descriptions(T).


sca_goal(ConvGoal,D) :-
	findall(D,(member(P,ConvGoal),
		   print('.'), debug_print(treat(P)), debug_nl,
	 	   sca_all_pred(P,D)),
	        Ds),
	max_desc(Ds,D).
	
sca(D) :-
	findall(D,(user_defined_predicate(P),
		   /* print('treating: '), print(P), nl, */
	 	   sca_all_pred(P,D)),
	        Ds),
	max_desc(Ds,D).

user_defined_predicate(pred(Pred,Arity)) :-
	defined_predicate(Pred,Arity).

sca_all_pred(P,D) :-
	findall(D,sca_pred(c(P,[]),D),Ds),	
	max_desc(Ds,D),
	cond_assert(memo(cm(P,[],D))).

sca_pred(c(P,Dp),D) :-
	seenB4(cm(P,Dp,D)), !.
sca_pred(c(P,Dp),[d(P,1)]) :-
	member(P,Dp), !.
sca_pred(c(P,Dp),D) :-
	user_defined_predicate(P),
	sc_clause(P,B),
	sort([P|Dp],NewDp),
	update_dep(NewDp,B,Cs),
	sca_preds(Cs,Ds),
	combine_desc(Ds,D1),
	max_desc([[d(P,1)],D1],D),
	cond_assert(memo(cm(P,Dp,D))).
sca_pred(c(P,Dp),[d(P,1)]) :-
	not(user_defined_predicate(P)).
	

sca_preds([],[]).
sca_preds([C|Cs],[D|Ds]) :-
	sca_pred(C,D),
	sca_preds(Cs,Ds).

max_desc([D],D).
max_desc([D|Ds],NewD1) :-
	max_desc(Ds,NewD),
	update_max_desc(D,NewD,NewD1).

update_max_desc([],D,D).
update_max_desc([d(P,N)|Ds],OldD,NewD1):-
	max_desc_elm(P,N,OldD,NewD),
	update_max_desc(Ds,NewD,NewD1).

max_desc_elm(P,N,[],[d(P,N)]).
max_desc_elm(P,N,[d(P,M)|D],[d(P,N)|D]) :- 
	N > M, !.
max_desc_elm(P,N,[d(P,M)|D],[d(P,M)|D]) :- !.
max_desc_elm(P,N,[DE|OldD],[DE|NewD]) :-
    	max_desc_elm(P,N,OldD,NewD).

combine_desc([],[]).
combine_desc([D],D).
combine_desc([D|Ds],NewD1) :-
	combine_desc(Ds,NewD),
	update_desc(D,NewD,NewD1).

update_desc([],D,D).
update_desc([d(P,N)|Ds],OldD,NewD1):-
	add_desc_elm(P,N,OldD,NewD),
	update_desc(Ds,NewD,NewD1).

add_desc_elm(P,N,[],[d(P,N)]).
add_desc_elm(P,N,[d(P,M)|D],[d(P,O)|D]) :- !,
	O is N + M.
add_desc_elm(P,N,[DE|OldD],[DE|NewD]) :-
    	add_desc_elm(P,N,OldD,NewD).

update_dep(Ds,[],[]).
update_dep(Ds,[P|Ps],[c(P,Ds)|C]) :-
	update_dep(Ds,Ps,C).

cond_assert(G) :- 
  call_built_in(G),!.
cond_assert(G) :- 
  assert(G).

initialize :-
	retractall(memo(cm(_,_,_))),
	retractall(sc_pred_count(_,_)).	

seenB4(C) :- memo(C).



generate_my_program :-
	retract(sc_clause(_,_)),
	fail.
generate_my_program :-
	claus(Nr,Head,Body),
	convert_literal(Head,CH),
	convert_body(Body,CB),
	assertz(sc_clause(CH,CB)),
	fail.
generate_my_program.


convert_literal(Lit,CL) :-
	is_built_in_literal(Lit),!,
	get_predicate(Lit,CL).
convert_literal(Lit,CL) :-
	is_negative_literal(Lit,Atom),!,
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

