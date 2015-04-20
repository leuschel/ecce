/* A Model Checker for CTL fomulas */
/* written for XSB-Prolog */
/* by Michael Leuschel, Thierry Massart */

/* tries to compute witnesses/counter-examples as well */


/* :- table sat/2. */

sat(State,Formula) :- sat(State,Formula,_Trace).

/* :- table sat/3. */

sat(_E,true,[]).
sat(_E,false,_) :- fail.

sat(E,p(P),[p(P)]) :- prop(E,P). /* elementary proposition */
sat(E,e(A),[e(A)]) :- trans(A,E,_). /* action A enabled in state E */

sat(E,and(F,G),[and(T1,T2)]) :- sat(E,F,T1), sat(E,G,T2).

sat(E,or(F,_G),T) :- sat(E,F,T).
sat(E,or(_F,G),T) :- sat(E,G,T).

sat(E,not(F),[not(F)]) :- tnot(sat(E,F)).

sat(E, imp(A,B),T) :- sat(E, or(not(A),B),T).

sat(E,en(F),[Act|T]) :- /* exists next */
    trans(Act,E,E2),sat(E2,F,T).

sat(E,an(F),[an(F)]) :- /* always next */
    tnot(sat(E,en(not(F)))).

sat(E,eu(F,G),T) :- /* exists until */
    sat_eu(E,F,G,T).

sat(E,au(F,G),[au(T1)]) :- /* always until */
  sat(E,not(eu(not(G),and(not(F),not(G)))),T1),
  sat_noteg(E,not(G)).

sat(E,ef(F),T) :- /* exists future */
 sat(E,eu(true,F),T).

sat(E,af(F),af(F)) :- /* always future */
  sat_noteg(E,not(F)).

sat(E,eg(F),[eg(F)]) :- /* exists global */
  tnot(sat_noteg(E,F)). /* we want gfp -> negate lfp of negation */

sat(E,ag(F)) :- /* always global */
  sat(E,not(ef(not(F)))).
  

/* :- table sat_eu/4. */
/* tabulation to compute least-fixed point using XSB */
  
sat_eu(E,_F,G,[holds(G,T)]) :- /* exists until */
    sat(E,G,T).
sat_eu(E,F,G,[Act|T]) :- /* exists until */
   sat(E,F),
   trans(Act,E,E2), sat_eu(E2,F,G,T).


/* :- table sat_noteg/2. */
/* tabulation to compute least-fixed point using XSB */

sat_noteg(E,F) :-
   sat(E,not(F)).
sat_noteg(E,F) :-
   findall(E2,trans(_A,E,E2),Succs),
   sat_noteg2(Succs,F).
   
sat_noteg2([],_).
sat_noteg2([S1|T],F) :-
   sat_noteg(S1,F),
   sat_noteg2(T,F).   
  
  
  /* ------------------------------------------------ */
  
  
le(X,X).
le(X,s(Y)) :- le(X,Y).

trans(t0,[think,P2,A1,A2,T,S], [wait,P2,T,A2,s(T),S]).
trans(t1,[wait,P2,A1,A2,T,S], [use,P2,A1,A2,T,S]) :- le(A1,S).
trans(t2,[use,P2,A1,A2,T,S], [think,P2,A1,A2,T,s(S)]).
trans(t3,[P1,think,A1,A2,T,S], [P1,wait,A1,T,s(T),S]).
trans(t4,[P1,wait,A1,A2,T,S], [P1,use,A1,A2,T,S]) :- le(A2,S).
trans(t5,[P1,use,A1,A2,T,S], [P1,think,A1,A2,T,s(S)]). 
 

/* The basic properties */
prop([use,use,A1,A2,T,S],unsafe).
prop([wait,P2,A1,A2,T,S],trying1).
prop([use,P2,A1,A2,T,S],satisfied1).

/* The start state for the animator and for backwards analysis */
start([think,think,0,0,0,0]).



test(T) :- start(S), sat(S,ef(p(unsafe)),T).  /*  <----- SLICING CRITERION */
