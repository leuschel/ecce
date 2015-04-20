
/* the below causes problems, because trans can take an arbitrary
   number of steps to compute. Hence, we need something like
   bottom-up success information inference for trans */

unsafe :- unsafe(agent(p)).
unsafe(State) :- print(chk(State)),nl,trans(b,State,_).
unsafe(State) :- trans(Action,State,NewState),unsafe(NewState).
trans(A,prefix(A,X),X).
trans(A,par(X,Y),par(X1,Y)) :- trans(A,X,X1).
trans(A,par(X,Y),par(X,Y1)) :- trans(A,Y,Y1).
trans(A,agent(X),X1) :- agent(X,XDef),trans(A,XDef,X1).
agent(p,  prefix(a, par(agent(p),agent(p)) )). 


unsafe2 :- unsafe2(agent(p)).

unsafe2(prefix(b,_)).
unsafe2(prefix(A,X)) :- unsafe2(X).
unsafe2(agent(p)) :- unsafe2(prefix(a, par(agent(p),agent(p)))).
unsafe2(par(X,Y)) :- unsafe2(X).
unsafe2(par(X,Y)) :- unsafe2(Y).


unsafe3(prefix(b,_),_,_).
unsafe3(prefix(A,X),X,Acc) :- print(p(Acc)),nl,unsafe3(Acc,NewAcc,NewAcc).
unsafe3(par(X,Y),par(Hole,Y),Acc) :- unsafe3(X,Hole,Acc).
unsafe3(par(X,Y),par(X,Hole),Acc) :- unsafe3(Y,Hole,Acc).
unsafe3(agent(p),prefix(a,par(agent(p),agent(p))),Acc) :- 
   print(a(Acc)),nl,unsafe3(Acc,NewAcc,NewAcc).



/* a special process algebra where the successor relation can be
  computed in a fixed number of steps */


unsafe4(X) :- trans4(X,c,_).
unsafe4(X) :- trans4(X,_,Y), unsafe4(Y).

trans4(prefix(A,X),A,X).
trans4(agent(X),tau,X1) :- agent4(X,X1).
trans4(choice(X,_),tau,X).
trans4(choice(_,X),tau,X).
trans4(bang(X,S),X,S).
trans4(bang(X,S),tau,bang(X,bang(X,S))).


agent4(p,choice(prefix(a,agent(p)),prefix(b,agent(p)))).
agent4(q,bang(a,prefix(b,agent(q)))).


unsafe5 :- unsafe5(agent(q)).
unsafe5(prefix(c,_)).
unsafe5(bang(c,_)).
unsafe5(prefix(_,X)) :- unsafe5(X).
unsafe5(choice(X,_)) :- unsafe5(X).
unsafe5(choice(_,X)) :- unsafe5(X).
unsafe5(agent(p)) :- unsafe5(choice(prefix(a,agent(p)),prefix(b,agent(p)))).
unsafe5(agent(q)) :- unsafe5(bang(a,prefix(b,agent(q)))).
unsafe5(bang(_,X)) :- unsafe5(X).
unsafe5(bang(X,S)) :- unsafe5(bang(X,bang(X,S))).

unsafe6 :- unsafe5(agent(p)).
