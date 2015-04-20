
rotateprune:rp(T1,T2) :- rotateprune:rotate(T1,U), rotateprune:prune(U,T2).

rotateprune:rotate(leaf(N),leaf(N)).
rotateprune:rotate(tree(L,N,R),tree(RL,N,RR)) :- 
	rotateprune:rotate(L,RL), rotateprune:rotate(R,RR).
rotateprune:rotate(tree(L,N,R),tree(RR,N,RL)) :-
	rotateprune:rotate(L,RL), rotateprune:rotate(R,RR).


rotateprune:prune(leaf(N),leaf(N)).
rotateprune:prune(tree(L,0,R),leaf(0)).
rotateprune:prune(tree(L,s(N),R),tree(PL,s(N),PR)) :- 
	rotateprune:prune(L,PL), rotateprune:prune(R,PR).