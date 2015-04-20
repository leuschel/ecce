:- mode rp(i,o).
rp(T1,T2) :- rotate(T1,U), prune(U,T2).

:- mode rotate(i,o).
rotate(leaf(N),leaf(N)).
rotate(tree(L,N,R),tree(RL,N,RR)) :- rotate(L,RL), rotate(R,RR).
rotate(tree(L,N,R),tree(RR,N,RL)) :- rotate(L,RL), rotate(R,RR).

:- mode prune(i,o).
prune(leaf(N),leaf(N)).
prune(tree(L,0,R),leaf(0)).
prune(tree(L,s(N),R),tree(PL,s(N),PR)) :- prune(L,PL), prune(R,PR).


depth( true, 0 ).
depth( (_g1,_gs), _depth ) :-
    depth( _g1, _depth_g1 ),
    depth( _gs, _depth_gs ),
    max( _depth_g1, _depth_gs, _depth ).
depth( _goal, s(_depth) ) :-
    prog_clause( _goal, _body ),
    depth( _body, _depth ).

max( _num, 0, _num ).
max( 0, s(_num), s(_num) ).
max( s(_x), s(_y), s(_max) ) :-
    max( _x, _y, _max ).

prog_clause( rotate(leaf(N),leaf(N)), true).
prog_clause( rotate(tree(L,N,R),tree(RL,N,RR)), (rotate(L,RL), rotate(R,RR))).
prog_clause( rotate(tree(L,N,R),tree(RR,N,RL)), (rotate(L,RL), rotate(R,RR))).
