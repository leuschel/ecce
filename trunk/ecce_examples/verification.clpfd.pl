
:- use_module(library(clpfd)).

/* p(_,X,[X]) :- X #< 0. */
p(b,_,_).

p(a,X,L) :- X #= 7, p(b,X,L).

p(a,X,[X|T]) :- Z #= X+2, p(a,Z,T).

p(b,X,[X|T]) :- Z #= X-1, p(b,Z,T).

/* p(a,X,[X|T]) :- Z #= X-4, p(a,Z,T). */



verify(X,L) :- p(a,X,L), labeling([],L).

verify1(X,L) :- X #= 2*N, N#>(-1), p(a,X,L).
verify2(X,L) :- X#= 2*N, N in inf..sup,p(a,X,L).

test(X) :- X#>1, p(X).
test2(X) :- X#<8, p(X).
test3(X) :- X#>8, p(X).

p(X) :- X #=8.
p(X) :- Z #= X+1, p(Z).



pp(X,X).
pp(X,Y) :- V #= X+1, W #= Y-1, pp(V,W).

test4(X,Y) :- X#>8, Y#>8, pp(X,Y).
test5(X,Y) :- X#<8, Y#<8, pp(X,Y).
test6(X,Y) :- X#>8, Y#<8, pp(X,Y).