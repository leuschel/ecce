:- module(apd_new,[instantiate_body/1, covered/2, tp_step/0, add_conjunction/1, rl/0]).

/* use_module('/Users/mal/cvs_root/ecce/ecce_source/abstractpd/apd_new.pl'). */

rl :- use_module('/Users/mal/cvs_root/ecce/ecce_source/abstractpd/apd_new.pl').

:- dynamic conjunction/2.

:- dynamic answer/2.

:- dynamic resultants/2.

:- dynamic change/1.

:- use_module(library(lists)).


conjunction(1,[p(_X,Y),q(Y,_Z)]).
conjunction(2,[t(_X)]).
conjunction(3,[u(_)]).

/* answer(1,[p(X,X),q(X,0)]).
answer(2,[t(_X)]). */

resultants(1, [clause([p(X,X),q(X,0)],[])]).
resultants(2, [clause([t(f(X,Z))],[p(X,b),q(b,Z)]), clause([t(f(a,0))],[])]).
resultants(3, [clause([u(X)],[t(X)])]).


:- dynamic conj_nr/1.

conj_nr(4).

get_conj_nr(X) :-
  retract(conj_nr(X)),
  X1 is X+1,
  assert(conj_nr(X1)).

add_conjunction(Q) :-
    get_conj_nr(N), assert(conjunction(N,Q)),
	copy_term(Q,CQ),
	user:varlist(CQ,TopGoalVarlist),
	user:calc_chtree(CQ,TopGoalVarlist,[],Chtree),
	findall(clause(CQ,Body),user:resultant_body(Chtree,dummy_node_id,CQ,Body),Resultants),
	print(Resultants),nl,
	assert(resultants(N,Resultants)).


subconjunctions([],[[]]).
subconjunctions([Literal|Q],Res) :-
  subconjunctions(Q,SQ),
  add_literal(Literal,SQ,Res).
  
add_literal(_L,[],[]).
add_literal(L,[Conj|T],[ [L|Conj], Conj |AT]) :-
  add_literal(L,T,AT).
  
subconjunction([],[]).
subconjunction([Literal|Q],Sub) :-
  subconjunction(Q,SQ),
  ((Sub = [Literal|SQ]) ; (Sub = SQ)).
  
 
/* check to see if a conjunction is covered by a global conjunction */
covered(ID,Conjunction) :-
  copy_term(Conjunction,CC),
  numbervars(CC,1,_),
  conjunction(ID,CC).
  
instantiate_body(Body) :-
   subconjunctions(Body,Subs),
   instantiate(Subs).

instantiate([]).
instantiate([Sub|T]) :-   
   ( covered(ID,Sub)
     -> (print(cov(ID,Sub)),nl,answer(ID,Sub))
      ; true),
   instantiate(T).
   
   
instantiate_resultants([],[]).
instantiate_resultants([clause(_H,B)|T],Res) :-
   (instantiate_body(B)
      -> (Res = [clause(_H,B)|IT])
       ; (Res = IT)), /* instantiate failed -> remove this resultant */
   instantiate_resultants(T,IT).
   
   
msg_of_heads([clause(H,_B)],H).
msg_of_heads([clause(H,_),clause(H2,_)|T],R) :-
   user:msg(H,H2,MSG),
   msg_of_heads([clause(MSG,[])|T],R).

tp_fix :- tp_step, (change(_) -> tp_fix ; (print('Fixpoint reached'),nl)).

tp_step :-
    retractall(change(_)),
    conjunction(ID,_Q),
    resultants(ID,Resultants),
    instantiate_resultants(Resultants,PrunedResultants),
    /* findall(H,member(clause(H,_),Resultants),Heads),
    print(heads(Heads)),nl,*/
    msg_of_heads(PrunedResultants,MSG),
    print(msg(MSG)),nl,
    ((retract(answer(ID,_A)),user:is_variant_of(MSG,_A))
      -> true ; (assert(change(ID)),print(change(ID)),nl)),
    assert(answer(ID,MSG)),
    fail.
tp_step :- print('done'),nl.
    
:- tp_fix.

   /* instantiate_body([p(X,b),q(b,Z)]). */