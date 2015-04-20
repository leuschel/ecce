:- module(qu_evan_and_user,[test/0,queen/2],[andorra]).

:- use_module(library(prolog_sys),[statistics/2]).
:- use_module(library(write),[write/1]).



test:- queen(8,G), fail.

:- determinate(queen(_N,_A),true).

queen(N,A) :- gen(N,L), queen2(L,[],A).

%% :- determinate(queen2(A,B,C), ( nonvar(A) ; B?\=C ) ).
%:- determinate(queen2(A,B,C), ( nonvar(A) ; nonvar(B),nonvar(C),\+(B=C) ) ).
:- determinate(queen2(A,B,C), ( nonvar(A) ; nonvar(B),nonvar(C),B?\=C ) ).

queen2([],R,R).
queen2([H|T],R,P) :-
    del(A,[H|T],L),        %
    safe(R,A,1),           % full parallelism here?  if so, what is
    queen2(L,[A|R],P).     % programmer's model?

:- determinate(safe(A,_B,_C),nonvar(A)).

safe([],_,_).
safe([H|T],U,D) :-
    G is H+D,
    G \== U,
    F is H-D,
    F \== U,
    D1 is D+1,
    safe(T,U,D1).

%:- determinate(del(A,B,C), ( nonvar(A),nonvar(B),B=[H|_],nonvar(H),\+(H=A) ;
%                             nonvar(C), \+(C=[_|_]) )).
:- determinate(del(A,B,C), ( nonvar(A),nonvar(B),A ?\= term(B,[1]) ; nonvar(C), C ?\= [_|_])).

del(X,[X|Y],Y).
del(X,[Y|Z],[Y|W]) :- del(X,Z,W).

:- determinate(gen(A,B), ( nonvar(A) ; nonvar(B) ) ).

gen(0,[]) :- !.
gen(N,[N|X]) :- M is N-1, gen(M,X).

/* 
prolog

108580 millisecons
reductions: 109412
fail:       7657
total:      117069

93260 millisecons
reductions: 99545
fail:       5509
total:      105054

S 2
77170 millisecons
reductions: 102787
fail:       5588
total:      108375

no output

S 0
91110 millisecons
reductions: 98902
fail:       5509
total:      104411

S 1
79080 millisecons
reductions: 100655
fail:       5557
total:      106212
 */
%%%%%%%%%%%%%%%%%%%%%

ourmain:-
	statistics(runtime,_),
	ourdo,
	statistics(runtime,[_,T1]),
        write(T1).

:- determinate(ourdo,true).

ourdo:-
	queen(8,_),
	fail.
ourdo.
