/* An old Lam&Kusalik benchmark */
/* the vanilla interpreter with depth of resolution information */
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

prog_clause( member( _X, _Xs ), append( _, [_X|_], _Xs ) ).
prog_clause( append( [], _L, _L ), true ).
prog_clause( append( [_X|_L1], _L2, [_X|_L3] ), append( _L1, _L2, _L3 ) ).

/* some sample specialization queries: */
test1(X,L,D) :- depth(member(X,L),D).
test2(X,D) :- depth(member(X,[a,b,c,m,d,e,m,f,g,m,i,j]),D).

app1(X,Y,R) :- depth(append(X,Y,R),_D).
  /* see how you can specialize this into append with Ecce; the RAF/FAR filtering
    will remove the unused Depth argument */
