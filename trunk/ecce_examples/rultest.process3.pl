
trans(prefix(A,X),A,X).
trans(interleave(X,Y),A,interleave(X2,Y)) :- trans(X,A,X2).
trans(interleave(X,Y),A,interleave(X,Y2)) :- trans(Y,A,Y2).
trans(choice(X,_Y),A,X2) :- trans(X,A,X2).
trans(choice(_X,Y),A,Y2) :- trans(Y,A,Y2).
trans(agent(X),A,Y) :- agent_def(X,Def), trans(Def,A,Y).


agent_def(apar,prefix(a,interleave(agent(apar),agent(apar)))).
agent_def(aparb,choice(prefix(b,stop),prefix(a,interleave(agent(aparb),agent(aparb))))).
agent_def(abc,prefix(a,choice(agent(abc),prefix(b,prefix(c,stop))))).



/* prefix traces: */
ptrace(_X,[]).
ptrace(X,[A|T]) :- trans(X,A,Y), ptrace(Y,T).


test1(T) :- ptrace(agent(apar),T).
test2(T) :- ptrace(agent(abc),T).
test3(T) :- ptrace(agent(aparb),T).

/* try to find a complete trace that can be done by both:
  should fail (infinitely) */
test4(T) :- trace(agent(aparb),T), trace(agent(abc),T).


stop(stop).
stop(interleave(X,_Y)) :- stop(X).
stop(interleave(_X,Y)) :- stop(Y).
stop(choice(X,_Y)) :- stop(X).
stop(choice(_X,Y)) :- stop(Y).

trace(X,[]) :- stop(X).
trace(X,[A|T]) :- trans(X,A,Y), trace(Y,T).

ls([]).
ls([_|T]) :- ls(T).



