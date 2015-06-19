:- module(homeomorphic,[homeomorphic_embedded_conjunction/2,not_more_general_conjunction/2,mixtus_term_size_embedded/2,mixtus_term_size/2,homeomorphic_embedded/2,chtree_homeomorphic_embedded/2,gen_sub_term/2,print_c/1,term_nesting_level/3,chtree_functor/1]).

/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,1996,1997 */
/* --------------------------------------------- */

/* TO GET THE LATEST VERSION OF THE ECCE SYSTEM GO TO:
	http://www.cs.kuleuven.ac.be/~michael */
/* BUG REPORTS, QUESTIONS AND COMMENTS CAN BE SENT TO:
	michael@cs.kuleuven.ac.be */


/* file: homemorphic.pro */


:- use_module(bimtools).
:- use_module(calc_chtree).
:- use_module(static_dynamic_functors).

:- use_package( .('ecce_no_rt') ).

% :- use_module(dynpreds).

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(self_check).

/* ===================================================== */

:- include( multi_meta ).

/* ===================================================== */

/* ----------------------------------- */
/* homeomorphic_embedded_conjunction/2 */
/* ----------------------------------- */

self_check(must_fail(homeomorphic_embedded_conjunction([a,q(X,Y)],
					[s,q(Y,f(X)),p,f(a),q(a,b)]))).
self_check(must_succeed(homeomorphic_embedded_conjunction([q(X,Y)],[q(Y,f(X))]))).
self_check(must_succeed(homeomorphic_embedded_conjunction([a,q(X,_Y)],
					[s,f(a),q(_Y,f(X)),p]))).
self_check(must_succeed(homeomorphic_embedded_conjunction([q(1,2),q(X,Y)],
			[q(1,2),q(2,3),q(f(1),f(2)),f(a),q(Y,f(X)),p]))).
self_check(must_succeed(homeomorphic_embedded_conjunction([],
					[s,f(a),q(_Y,f(_X)),p]))).
self_check(must_succeed(homeomorphic_embedded_conjunction([q(X,Y)],
					[q(X,Y)]))).
self_check(must_succeed(homeomorphic_embedded_conjunction([q(a,Y)],
					[q(b,Y),q(c,Y),q(a,Y)]))).
self_check(must_succeed(homeomorphic_embedded_conjunction([q(a,Y)],
					[q(b,Y),q(a,Y),q(c,Y)]))).

homeomorphic_embedded_conjunction([],_).
homeomorphic_embedded_conjunction([PA|PAs],[A|As]) :-
  homeomorphic_embedded(PA,A),
   !, /* added by leuschel: 29/4/05 */
  homeomorphic_embedded_conjunction(PAs,As).
homeomorphic_embedded_conjunction([PA|PAs],[_|As]) :-
  homeomorphic_embedded_conjunction([PA|PAs],As).

/* ------------------------------ */
/* not_more_general_conjunction/2 */
/* ------------------------------ */

not_more_general_conjunction([],_).
not_more_general_conjunction([PA|PAs],[A|As]) :-
  atoms_have_same_predicate(PA,A,_),
  \+(strict_instance_of(PA,A)),
  mixtus_term_size_embedded(PA,A),
  not_more_general_conjunction(PAs,As).
not_more_general_conjunction([PA|PAs],[_|As]) :-
  not_more_general_conjunction([PA|PAs],As).


mixtus_term_size_embedded(PA,A) :-
  mixtus_term_size(PA,PSize),
  mixtus_term_size(A,ASize),
  PSize =< ASize.

/* ------------------ */
/* mixtus_term_size/2 */
/* ------------------ */

mixtus_term_size(X,1) :- var(X),!.
mixtus_term_size(X,1) :- atomic(X),!.
mixtus_term_size(X,Size) :- 
	X =..[_Pred|Args],
	l_mixtus_term_size(Args,1,Size).

l_mixtus_term_size([],S,S).
l_mixtus_term_size([H|T],InS,OutS) :-
	mixtus_term_size(H,HS),
	IntS is InS + HS,
	l_mixtus_term_size(T,IntS,OutS).



/* ----------------------- */
/* homeomorphic_embedded/2 */
/* ----------------------- */

self_check(must_fail(homeomorphic_embedded(p(f(a)),p(a)))).
self_check(must_fail(homeomorphic_embedded(p(a),p(_X)))).
self_check(must_fail(homeomorphic_embedded(p(a),p(p(f(g(b,c))))))).
self_check(must_fail(homeomorphic_embedded(f(a,b),f(g(a,b),c)))).
self_check(must_succeed(homeomorphic_embedded(a,f(a)))).
self_check(must_succeed(homeomorphic_embedded(f(X),f(X)))).
self_check(must_succeed(homeomorphic_embedded(f(X,Y),f(Y,X)))).
self_check(must_succeed(homeomorphic_embedded(f(X,Y),f(Y,s(X))))).
self_check(must_succeed(homeomorphic_embedded(f(a,_Y),f([c,b,a],s(_X))))).
self_check(must_succeed(homeomorphic_embedded(f(f(a)),f(f(a))))).
self_check(must_succeed(homeomorphic_embedded(q(b),q(1,2,q(f(1,f(b,2,3),3)))))).
self_check(must_succeed(homeomorphic_embedded(q(b),q(1,q(b),b)))).
self_check(must_succeed(homeomorphic_embedded(q(X),f(a,q(s(X)))))).
self_check(must_succeed(homeomorphic_embedded(p(1,2,f(3)),
				p(1,p(s(a,1),f(f(2)),g(a,b,f(3))),3)))).
self_check(must_succeed(homeomorphic_embedded(q(_X,Y),q(Y,f(_X))))).

/* homeomorphic_embedded(X,Y) :- print(homeomorphic_embedded(X,Y)),nl,fail. */

/* homeomorphic_embedded(X,Y) :- var(X),!,
 (nonvar(Y) -> (print(h(X,Y)),nl) ; true). */
homeomorphic_embedded(X,Y) :- var(X),var(Y),!.
homeomorphic_embedded(_X,Y) :-
	var(Y),!,fail.
homeomorphic_embedded(X,Y) :-
	nonvar(X),dynamic_term(X),
	nonvar(Y),dynamic_term(Y),!.
homeomorphic_embedded(X,Y) :-
	strict_instance_of(X,Y),!,print('$*'),debug_print('$'(X,Y)),fail.
homeomorphic_embedded(X,Y) :- /* coupling for unary constructors */
	nonvar(X),nonvar(Y),
	X=..[Func,XArg],
	Y=..[Func,YArg],
	!, /* do not try diving for unary matching constructors */
	homeomorphic_embedded(XArg,YArg),!.
homeomorphic_embedded(X,Y) :- /* coupling */
	nonvar(X),nonvar(Y),
	X=..[Func|XArgs],
	Y=..[Func|YArgs],
	l_homeomorphic_embedded(XArgs,YArgs),!.
homeomorphic_embedded(X,Y) :- /* diving */
	nonvar(Y),
	term_nesting_level(X,NX,SumX),
	gen_sub_term(Y,Sub),
	term_nesting_level(Sub,NSub,SumSub),
	NSub>=NX,
	SumSub>=SumX,
	/*print(gen_sub_term(Y,Sub)),nl,*/
	homeomorphic_embedded(X,Sub),!.

/* l_homeomorphic_embedded(X,Y) :- 
	print(l_homeomorphic_embedded(X,Y)),nl,fail. */
l_homeomorphic_embedded([],[]).
l_homeomorphic_embedded([X|TX],[Y|TY]) :-
	homeomorphic_embedded(X,Y),!,
	l_homeomorphic_embedded(TX,TY).


/* ------------------------------ */
/* chtree_homeomorphic_embedded/2 */
/* ------------------------------ */

:- dynamic homeo_count/1.
homeo_count(0). /* counts the number of coupling operations */

reset_homeo_count :-
	retract(homeo_count(_X)),
	assert(homeo_count(0)).


chtree_homeomorphic_embedded(X,_Y) :-
	is_inf(X),print('### infinite cyclic term X'),nl,!.
chtree_homeomorphic_embedded(_X,Y) :-
	is_inf(Y),print('### infinite cyclic term Y'),nl,!.
chtree_homeomorphic_embedded(X,Y) :-
	reset_homeo_count,
	term_nesting_level(X,NX,SumX),
	term_nesting_level(Y,NY,SumY),
	NY>=NX,
	SumY>=SumX,
	chtree_homeomorphic_embedded2(X,Y).


chtree_homeomorphic_embedded2(X,Y) :- var(X),var(Y),!.
chtree_homeomorphic_embedded2(X,Y) :-
	nonvar(X),dynamic_term(X),\+(chtree_functor(X)),
	nonvar(Y),dynamic_term(Y),\+(chtree_functor(Y)),!.
chtree_homeomorphic_embedded2(X,Y) :-
	nonvar(X),nonvar(Y),
	X=..[Func|XArgs],
	Y=..[Func|YArgs],
	retract(homeo_count(HC)),
	HC1 is HC+1,assert(homeo_count(HC1)),
	(HC>10000
	  -> (HC = 10001 -> print('[Db]') ; true)
	  ;  l_chtree_homeomorphic_embedded(XArgs,YArgs)
	),!.
chtree_homeomorphic_embedded2(X,Y) :-
	/* term_nesting_level(X,NX,SumX), */
	gen_sub_term(Y,Sub),
	/* term_nesting_level(Sub,NSub,SumSub),
	NSub>=NX,
	SumSub>=SumX, */
	/* print('|'), */
	chtree_homeomorphic_embedded2(X,Sub),!.

l_chtree_homeomorphic_embedded([],[]).
l_chtree_homeomorphic_embedded([X|TX],[Y|TY]) :-
	chtree_homeomorphic_embedded2(X,Y),!,
	l_chtree_homeomorphic_embedded(TX,TY).

gen_sub_term(X,Sub) :-
	nonvar(X),
	X=..[_F|Args],
	member(Sub,Args).

:- dynamic print_count/1.
print_count(0).

print_c(S) :-
  retract(print_count(C)),
  (C>1000
	-> (print(S),assert(print_count(0)))
	;  (C1 is C + 1, assert(print_count(C1)))
  ).


/* CHECK WHETHER THIS IS REALLY USEFUL */
/* term_nesting_level(_,0,0) :- !. */

term_nesting_level(X,0,0) :- var(X),!.
term_nesting_level(X,1,1) :- atomic(X),!.
term_nesting_level(X,N,S) :- nonvar(X),!,
	X=..[_F|Args],
	l_term_nesting_level(Args,NA,SA),
	N is NA + 1,
	S is SA + 1.

l_term_nesting_level([],0,0).
l_term_nesting_level([H|T],N,S) :-
	term_nesting_level(H,NH,SH),
	l_term_nesting_level(T,NT,ST),
	max(NH,NT,N),
	S is SH + ST.

chtree_functor(none).
chtree_functor(empty).
chtree_functor(stop).
chtree_functor(success).
chtree_functor(select(_Nr,_Chpaths)).
chtree_functor(remove(_Nr,_Pred,_Chtree)).
chtree_functor(built_in_eval(_Nr,_BuiltIn,_Chtree)).
chtree_functor(built_in_simplify(_Nr,_BuiltIn,_Chtree)).
chtree_functor(match(_Nr,_Chtree)).
chtree_functor([]).
chtree_functor([_H|_T]).

chtree_functor(stop(_)).
chtree_functor(empty_match(_VH)).
chtree_functor(MatchTerm) :-
	nonvar(MatchTerm),
	MatchTerm =.. [match,_VarHead,_ClauseNrs|_Matches].
chtree_functor(Nr) :-
	nonvar(Nr),
	claus(Nr,_Head,_Body).
