unsafe(C) :- unsafe(agent(p),C).
unsafe(State,_) :- print(chk(State)),nl,trans(b,State,_).
unsafe(State,s(Counter)) :-
   trans(Action,State,NewState),unsafe(NewState,Counter).
trans(A,prefix(A,X),X).
trans(A,par(X,Y),par(X1,Y)) :- trans(A,X,X1).
trans(A,par(X,Y),par(X,Y1)) :- trans(A,Y,Y1).
trans(A,agent(X),X1) :- agent(X,XDef),trans(A,XDef,X1).
agent(p,  prefix(a, par(agent(p),agent(p)) )).