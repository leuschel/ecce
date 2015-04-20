
trans(prefix(A,X),A,X).
trans(par(X,Y),A,par(X2,Y2)) :- trans(X,A,X2), trans(Y,A,Y2).
trans(interleave(X,Y),A,interleave(X2,Y)) :- trans(X,A,X2).
trans(interleave(X,Y),A,interleave(X,Y2)) :- trans(Y,A,Y2).
trans(choice(X,_Y),A,X2) :- trans(X,A,X2).
trans(choice(_X,Y),A,Y2) :- trans(Y,A,Y2).
trans(agent(X),A,Y) :- agent_def(X,Def), trans(Def,A,Y).


agent_def(u2,prefix(a,interleave(prefix(b,stop),agent(u2)))).
agent_def(v2,prefix(a,interleave(agent(v2),prefix(b,stop)))).
agent_def(w2,choice(prefix(b,stop),prefix(a,prefix(b,agent(w2))))).
agent_def(z2,prefix(a,interleave(agent(z2),agent(z2)))).


/* prefix traces: */
ptrace(_X,[]).
ptrace(X,[A|T]) :- trans(X,A,Y), ptrace(Y,T).


test1(T) :- ptrace(agent(z2),T).

test2(T) :- trace(agent(v2),T).
test3(T) :- trace(agent(w2),T).

/* try to find a complete trace that can be done by both:
  should fail (infinitely) */
test4(T) :- trace(agent(w2),T), trace(agent(v2),T).

/* td using John's RUL tool gives:
test4_ans(X1) :-t678(X1).
t678([]) :-true.
t678([X1|X2]) :-t39(X1),t678(X2).
t39(b) :-true.
t39(a) :-true.
*/

stop(stop).
stop(par(X,Y)) :- stop(X),stop(Y).
stop(interleave(X,_Y)) :- stop(X).
stop(interleave(_X,Y)) :- stop(Y).
stop(choice(X,_Y)) :- stop(X).
stop(choice(_X,Y)) :- stop(Y).

trace(X,[]) :- stop(X).
trace(X,[A|T]) :- trans(X,A,Y), trace(Y,T).

ls([]).
ls([_|T]) :- ls(T).


