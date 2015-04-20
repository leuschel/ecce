trans(prefix(A,X),A,X).
trans(par(X,Y),A,par(X2,Y2)) :- trans(X,A,X2), trans(Y,A,Y2).
trans(interleave(X,Y),A,interleave(X2,Y)) :- trans(X,A,X2).
trans(interleave(X,Y),A,interleave(X,Y2)) :- trans(Y,A,Y2).
trans(choice(X,Y),A,X2) :- trans(X,A,X2).
trans(choice(X,Y),A,Y2) :- trans(Y,A,Y2).
trans(agent(X),A,Y) :- agent_def(X,Def), trans(Def,A,Y).


agent_def(v2,prefix(a,interleave(agent(v2),prefix(b,stop)))).
agent_def(w2,choice(prefix(b,stop),prefix(a,prefix(b,agent(w2))))).
agent_def(z2,prefix(a,interleave(agent(z2),agent(z2)))).


tt1(agent(z2)).
tt1(interleave(X,Y)) :- tt1(X),tt1(Y).

test(X,Y) :- tt1(X),trans(X,_,Y).