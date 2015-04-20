
test([T1,T2]) :- rotate(T1,U), prune(U,T2). 
/* <-- specialize this; needs deforestation setting (to be added to interface) */

rotate(leaf(N),leaf(N)).
rotate(tree(L,N,R),tree(RL,N,RR)) :- 
	rotate(L,RL), rotate(R,RR).
rotate(tree(L,N,R),tree(RR,N,RL)) :-
	rotate(L,RL), rotate(R,RR).


prune(leaf(N),leaf(N)).
prune(tree(L,0,R),leaf(0)).
prune(tree(L,s(N),R),tree(PL,s(N),PR)) :- 
	prune(L,PL), prune(R,PR).