
eval(prefix(A,X),[A|T]) :- eval(X,T).
eval(choice(X,_Y),T) :- eval(X,T).
eval(choice(_X,Y),T) :- eval(Y,T).
eval(stop,[]).
eval(par(X,Y),T) :- eval(X,T), eval(Y,T).
eval(agent(X),T) :- agent(X,Def), eval(Def,T).

agent(p, prefix(a,agent(p))).
agent(q, prefix(a, choice(stop, agent(q)))).

agent(r, par(agent(q), agent(q))).

agent(s, prefix(a, choice(prefix(b,stop), agent(s)))).

agent(t, choice(stop,prefix(a, par(agent(t),agent(t))))).

test(T) :- eval(agent(r),T).
test2(T) :- eval(par(agent(q),agent(s)),T).

test3(T) :- eval(agent(t),T).



p([c]).
p([b]).
p([a|X]):- p(X).
p([b|X]) :- p(X).

q([d]).
q([b]).
q([a|X]) :- q(X).

r([a]).
r([a|T]) :- r(T).
r([b|T]) :- r(T).
r([c|T]) :- r(T).

rev([],L,L).
rev([H|T],A,R) :- rev(T,[H|A],R).

t(X,R) :- p(X),q(X),rev(X,[],R),r(R).

t2(X) :- p(X),rev(X,[],X).

trans(prefix(A,X),A,X).
trans(par(X,Y),A,par(X2,Y2)) :- trans(X,A,X2), trans(Y,A,Y2).
trans(interleave(X,Y),A,interleave(X2,Y)) :- trans(X,A,X2).
trans(interleave(X,Y),A,interleave(X,Y2)) :- trans(Y,A,Y2).
trans(choice(X,Y),A,X2) :- trans(X,A,X2).
trans(choice(X,Y),A,Y2) :- trans(Y,A,Y2).
trans(agent(X),A,Y) :- agent_def(X,Def), trans(Def,A,Y).

stop(stop).
stop(par(X,Y)) :- stop(X),stop(Y).
stop(interleave(X,Y)) :- stop(X),stop(Y).
stop(choice(X,_Y)) :- stop(X).
stop(choice(_X,Y)) :- stop(Y).

agent_def(inf_as,choice(prefix(b,stop),prefix(a,agent(inf_as)))).
agent_def(p,choice(prefix(c,stop),prefix(a,par(agent(p),agent(p))))).
agent_def(q,choice(prefix(c,stop),prefix(a,agent(q)))).

agent_def(v,choice(stop,prefix(a,interleave(agent(v),prefix(b,stop))))).
agent_def(w,choice(prefix(b,stop),prefix(a,prefix(b,agent(w))))).

agent_def(v2,prefix(a,interleave(agent(v2),prefix(b,stop)))).
agent_def(w2,choice(prefix(b,stop),prefix(a,prefix(b,agent(w2))))).
agent_def(z2,prefix(a,interleave(agent(z2),agent(z2)))).

trace(X,[]) :- stop(X).
trace(X,[A|T]) :- trans(X,A,Y), trace(Y,T).

/* prefix traces: */
ptrace(X,[]).
ptrace(X,[A|T]) :- trans(X,A,Y), ptrace(Y,T).

test4(T) :- trace(agent(inf_as),T), trace(agent(q),T).
test5(T) :- trace(agent(inf_as),T), trace(agent(p),T).

test6(T) :- trace(agent(v),T), trace(agent(w),T).
test7(T) :- ptrace(agent(v2),T), ptrace(agent(w2),T).

 /* check whether exists trace common to both */
