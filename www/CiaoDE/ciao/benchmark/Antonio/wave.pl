 %% Modification of the General Program transform to use lists. This is
 %% program is only for testing efficiency. Do not use.  Compile with
 %% GNU-Prolog using gplc --min-size wave.pl


 %% A simple filter. With lists, it is very difficult to design filters
 %% that take more than two elements into account.  Therefore, the whole
 %% program works with two elements only. It should be OK with Fourier,
 %% Ferraz de Mello, Haar and a few other transforms.

:- use_module(library(prolog_sys)).
:- use_module(library(lists)).
:- use_package(hiord).

r(0.707106781186547517).

mf(X, Y, f(Y0, Y1)) :- r(R), Y0 is (X+Y)*R, Y1 is (X-Y)*R. 

% This version of applyFilter can take two-element
% filters. This should accomodate quite a few filters,
% like Malat, Haar, Daubeauchies, Ferraz de Mello, etc.
applyFilter([], _F, []).
applyFilter([X0, X1|XS], F, [Filtered|YS]) :-
%        filter(F, X0, X1, Filtered),
        Filter =.. [F, X0, X1, Filtered],
        call(Filter),
	applyFilter(XS, F, YS).

 %% filter(mf, X, Y, F) :- mf(X, Y, F).
 %% filter(dummy, X, Y, f(X,Y)).

% Here is the transform. 
% MCL: wv/3 halves the size in each step, so optimizing indexing will
% not be such a gain (only 16 recursive calls for the example).
% However, append/3 at the end should be a real killer!  I'm for a
% dif. list. implementation.

wv(_F, [X], [X|A]-A):- !.
wv(F, XS, YSa-YSb) :-
    applyFilter(XS, F, FS), 
    unzip(FS, MS, Z-YSb),
    wv(F, MS, YSa-Z).

% unzip was simplified to the case of two element filters.
unzip([], [], X-X).
unzip([f(X, Y)|FS], [X|XS], [Y|YS]-Zs) :-
   unzip(FS, XS, YS-Zs). 

% Euclidean distance between direct and reverse transformation.
% One dimensional case.
compare([], [], S, S).
compare([X|XS], [Y|YS], S1, S) :-
   Next is (X-Y)*(X-Y)+S1,
   compare(XS, YS, Next, S).


% Imitation of two dimensional transforms.  True two dimensional
% transforms is quite difficult without arrays (it is very inefficient
% to go through columns in lists of lists.

loop(M, M, M) :- !.
loop(I, _M, I).
loop(I, M, J) :- I1 is I+1,
        loop(I1, M, J).

% Fake processing of the transform.
% MCL: sign is a builtin arithmetic functor; can be used.  This is
% however just simulating a process, so it does not make much sense to
% change it.

proc([], []).
proc([X|XS], [Y|YS]) :-
   sign(X, S),
   Y is S*sqrt(X*X),
   proc(XS, YS).

sign(X,  1):- X > 0, !.
sign(X, -1):- X < 0.


% Create a fake signal.
makeExample(I, I, [I]):- !.
makeExample(I, J, [I|XS]):-
        I < J,
        I1 is I+1,
        makeExample(I1, J, XS).	

% Without arrays, it is very difficult the implementation of a general
% back transform. Here, I present the particular case of Haar
% transform. 
% MCL: Rewritten (yes, it is just a particular case, but the code can
% be really made more efficient).

cf(Sign, HaarA, HaarB, Result):-
        r(R), 
        cf2(HaarA, HaarB, Sign, R, Result).


cf2([X|Xs], [Y|Ys], Sign, R, [N|Ns]):- !,
        (
            Sign = 1 ->
            N is X*R - R*Y,
            cf2(Xs, Ys, 0, R, Ns)
        ;
            N is X*R + R*Y,
            cf2([X|Xs], [Y|Ys], 1, R, Ns)
        ).
cf2(_HaarA, _HaarB, _Sign, _R, []).
% 

% Reimplementation of drop.
drop([_|B], [_|A], R):-!, drop(B, A, R).
drop(_, A, A).

% Haar back transform
ahar([], This, _R, This, []).
ahar([HA|A], [HB|B], R, C, D) :-
        X is HB*R+HA*R,
        cf2([HB|B], [HA|A], 1, R, CF),
        drop(B, A, DROPED),
        ahar(DROPED, [X|CF], R, C, D).

% Calling Haar back transform
ahaar([X|XS], Res) :-
        r(R),
        ahar(XS, [X], R, Res, _).
   
% run uses the one dimensional transform to simulate bidimensional
% transformation. 

run(N, M, Filter) :-
   asserta(pr([])),
   loop(1, M, J), % goes through y dimension
   makeExample(1, N, Ex), % create x-line
   wv(Filter, Ex, Res1-[]), % apply transform
   proc(Res1, Res),
   ahaar(Res, Back),
   compare(Ex, Back, 0, Precision),
   retract(pr(P1)), 
   asserta(pr([f(J, Precision)|P1])), 
   fail.
run(_, _, _Filter) :-
  retract(pr(Ps)), write(Ps), nl.

main :-
        statistics(runtime, _),
        run(65536, 50, mf), 
        nl,
        statistics(runtime, [_,T]),
        write(total_time(T)), nl,
        statistics.

% :- initialization(q).
