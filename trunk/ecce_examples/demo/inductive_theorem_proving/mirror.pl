
mirror(tip,tip).
mirror(tree(L,N,R),tree(RR,N,RL)) :- mirror(L,RL), mirror(R,RR).

rr(X,R) :- mirror(X,Z),mirror(Z,R).