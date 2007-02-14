/* 

Readers & Writers : A classical example of Petri Net with inhibitor arcs.

Vars 
  7 
Rules 
  X1>=1 -> X1=X1-1,X2=X2+1 ;
  X2>=1 -> X2=X2-1,X3=X3+1 ;
  X2>=1 -> X2=X2-1,X4=X4+1 ;
  X3>=1,X5>=1 -> X3=X3-1,X6=X6+1 ;
  X4>=1,X5>=1,X6=0 -> X4=X4-1,X5=X5-1,X7=X7+1 ;
  X6>=1 -> X6=X6-1,X1=X1+1 ;
  X7>=1 -> X7=X7-1,X5=X5+1,X1=X1+1 ;
Initial state 
  X1>=1,X5=1,X2=0,X3=0,X4=0,X6=0,X7=0 
Target state 
  X6>=1,X7>=1 

*/

trans([s(X1),X2,X3,X4,X5,X6,X7],[X1,s(X2),X3,X4,X5,X6,X7]).
trans([X1,s(X2),X3,X4,X5,X6,X7],[X1,X2,s(X3),X4,X5,X6,X7]).
trans([X1,s(X2),X3,X4,X5,X6,X7],[X1,X2,X3,s(X4),X5,X6,X7]).
trans([X1,X2,s(X3),X4,s(X5),X6,X7],[X1,X2,X3,X4,s(X5),s(X6),X7]).
trans([X1,X2,X3,s(X4),s(X5),0,X7],[X1,X2,X3,X4,X5,0,s(X7)]).
trans([X1,X2,X3,X4,X5,s(X6),X7],[s(X1),X2,X3,X4,X5,X6,X7]).
trans([X1,X2,X3,X4,X5,X6,s(X7)],[s(X1),X2,X3,X4,s(X5),X6,X7]).


start([s(X1),0,0,0,s(0),0,0,0]). 
start(SK,S) :- S = [SK,0,0,0,s(0),0,0,0].

prop([X1,X2,X3,X4,X5,s(X6),s(X7)],unsafe).

/* forward search : */
forward :- start(X),forward(X).
forward(X):- prop(X,unsafe).
forward(X) :- trans(X,Y), forward(Y).

gen_int(0,0).
gen_int(X,s(Y)) :- X>0,X1 is X-1, gen_int(X,Y).

test(K) :- gen_int(K,SK),start(SK,State), forward(State).

