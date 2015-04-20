:- module(_,_,[assertions,regtypes]).

mul(0,X,0) :- pint(X).
mul(s(X),Y,Z) :- mul(X,Y,XY), plus(XY,Y,Z).

plus(0,X,X) :- pint(X).
plus(s(X),Y,s(Z)) :- plus(X,Y,Z).


test1(X,Z) :- mul(X,0,Z).  /* try to prove that Z = 0 */

commutativity_mul(X,Y,Z1,Z2) :- mul(X,Y,Z1), mul(Y,X,Z2).
commutativity_plus(X,Y,Z1,Z2) :- plus(X,Y,Z1), plus(Y,X,Z2).


functionality_plus(X,Y,Z1,Z2) :- plus(X,Y,Z1), plus(X,Y,Z2).
functionality_mul(X,Y,Z1,Z2) :- mul(X,Y,Z1), mul(X,Y,Z2).

:- regtype pint/1.

pint(0).
pint(s(X)) :- pint(X).


/*

We want to prove functionality of multiplication, i.e.,
mul(X,Y,Z1), mul(X,Y,Z2)   ---> Z1 = Z2

1. Unfold
mul(X,Y,Z1), mul(X,Y,Z2)


1a.
mul(0,Y,0),mul(0,Y,Z2).
mul(s(X),Y,Z1),mul(s(X),Y,Z2) :- mul(X,Y,XY), plus(XY,Y,Z1).

1b.
mul(0,Y,0),mul(0,Y,0).
mul(s(X),Y,Z1),mul(s(X),Y,Z2) :- mul(X,Y,XY), plus(XY,Y,Z1), mul(X,Y,XY2), plus(XY2,Y,Z2).

1c. (reorder)
mul(0,Y,0),mul(0,Y,0).
mul(s(X),Y,Z1),mul(s(X),Y,Z2) :- mul(X,Y,XY),mul(X,Y,XY2),  plus(XY,Y,Z1), plus(XY2,Y,Z2).


1. end of local control


2. (rename&filter for clarity)
mm(0,Y,0,0).
mm(s(X),Y,Z1,Z2) :- mm(X,Y,XY,XY2),  plus(XY,Y,Z1), plus(XY2,Y,Z2).


NOTE:
Analysing  plus(XY,Y,Z1), plus(XY2,Y,Z2)  will *never* yield Z1=Z2 !


3. Start bottom-up abstract interpretation
Initially:  S0 = {mm --> bot} 
                       [we could produce code for plus and start with bot]
                       
3a. Check whether everything is covered, instantiate resultants:
mm(0,Y,0,0).
mm(s(X),Y,Z1,Z2) :- fail.
==> ok, covered.

3b. we thus continue with Tp:
Tp(S0) = {mm --> mm(0,Y,0,0)}

---> note if we continue with Tp until fixpoint reached we will miss the boat,
     and *not* be able to prove functionality !!
     
4. Instantiate resultants based on TP(S0) and check coveredness:
mm(0,Y,0,0).
mm(s(0),Y,Z1,Z2) :- mm(0,Y,0,0),  plus(0,Y,Z1), plus(0,Y,Z2).

not covered, no specialised code for plus(0,Y,Z1), plus(0,Y,Z2).

5. Produce specialised conjunction and unfold: plus(0,Y,Z1), plus(0,Y,Z2)

plus(0,Y,Y),plus(0,Y,Y).

6. Continue with Tp
    S1 = {mm --> mm(0,Y,0,0), pp --> plus(0,Y,Y),plus(0,Y,Y)}
mm(0,Y,0,0).
mm(s(X),Y,Z1,Z2) :- mm(X,Y,XY,XY2),  plus(XY,Y,Z1), plus(XY2,Y,Z2).

    msg(mm(0,Y,0,0), mm(s(0),Y,Y,Y)) = mm(X,Y,Z,Z)
    Tp(S1) = {mm --> mm(X,Y,Z,Z), pp --> plus(0,Y,Y),plus(0,Y,Y)}
    
7. Instantiate using Tp(S1):
mm(0,Y,0,0).
mm(s(X),Y,Z1,Z2) :- mm(0,Y,XY,XY),  plus(XY,Y,Z1), plus(XY,Y,Z2).

8. Produce specialised conjunction for plus(XY,Y,Z1), plus(XY,Y,Z2)

plus(0,Y,Y),plus(0,Y,Y).
plus(s(XY),Y,s(Z1)), plus(s(XY),Y,s(Z2)) :- plus(XY,Y,Z1), plus(XY,Y,Z2).


9. rename & filter:

pp2(0,Y,Y,Y).
pp2(s(XY),Y,s(Z1),s(Z2)) :- pp2(XY,Y,Z1,Z2).


10. Putting everything together and continuing Tp we now reach a fixpoint:


mm(0,Y,0,0).
mm(s(X),Y,Z1,Z2) :- mm(X,Y,XY,XY2),  plus(XY,Y,Z1), plus(XY2,Y,Z2).
pp2(0,Y,Y,Y).
pp2(s(XY),Y,s(Z1),s(Z2)) :- pp2(XY,Y,Z1,Z2).

   S2 = {mm --> mm(X,Y,Z,Z), pp2 --> bot}
   
   S3 = {mm --> mm(X,Y,Z,Z), pp2 --> pp2(0,Y,Y,Y)}
    note if we allow Tp to be non-monotonic we get mm--> mm(0,Y,0,0) 
   
   S4 = {mm --> mm(X,Y,Z,Z), pp2 --> pp2(X,Y,Z,Z)}
   
   and we have reached the fixpoint and we can generate the code:
   
   
mm(0,Y,0,0).
mm(s(X),Y,Z,Z) :- mm(X,Y,XY,XY),  pp2(XY,Y,Z,Z).
pp2(0,Y,Y,Y).
pp2(s(XY),Y,s(Z),s(Z)) :- pp2(XY,Y,Z,Z).

after further filtering:


mm(0,Y,0).
mm(s(X),Y,Z) :- mm(X,Y,XY),  pp2(XY,Y,Z).
pp2(0,Y,Y).
pp2(s(XY),Y,s(Z)) :- pp2(XY,Y,Z).


Question: what exactly to do with uncovered conjunctions !! top/bot/unfold and then bot?
 can conjunctions be added to CiaoPP?
   
*/