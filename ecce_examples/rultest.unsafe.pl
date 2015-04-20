unsafe :- unsafe(agent(system)).
unsafe(State) :- deadlock(State).
unsafe(State) :- trans(_Action,State,NewState),unsafe(NewState).
 
 
trans(A,prefix(A,X),X).
trans(A,choice(X,_),X1) :- trans(A,X,X1).
trans(A,choice(_,X),X1) :- trans(A,X,X1).
trans(A,interleave(X,Y),interleave(X1,Y)) :- trans(A,X,X1).
trans(A,interleave(X,Y),interleave(X,Y1)) :- trans(A,Y,Y1).
trans(A,par(X,Y),par(X1,Y1)) :- trans(A,X,X1),trans(A,Y,Y1).
trans(tau,agent(X),XDef) :- agent(X,XDef).
 
 
deadlock(stop).
deadlock(interleave(X,Y)) :- deadlock(X),deadlock(Y).
deadlock(choice(X,_Y)) :- deadlock(X).
deadlock(choice(_X,Y)) :- deadlock(Y).
deadlock(par(X,Y)) :- \+(trans(_,par(X,Y),_)).
 
agent(p,  prefix(a, interleave(agent(p),agent(p)) )).
 
 agent(buff,  prefix(in, prefix(out, agent(buff)))).
 
 agent(server, choice(prefix(new_server, interleave(agent(server),agent(server))),
                      prefix(request,prefix(serve,agent(server))))).
 agent(client, prefix(request,prefix(serve,agent(client)))).
 agent(system, par(agent(client),par(agent(client),agent(server)))).