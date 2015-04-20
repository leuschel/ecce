% Modification of the General Program
% transform to use lists. This is program
% is only for testing efficiency. Do not use.
% Compile with GNU-Prolog using 
% gplc --min-size wave.pl


% A simple filter. With lists, it is very
% difficult to design filters that take
% more than two elements into account.
% Therefore, the whole program works with
% two elements only. It should be OK with
% Fourier, Ferraz de Mello, Haar and a few
% other transforms.

:- use_module(library(prolog_sys)).
:- use_module(library(lists)).

r(X) :- X is 1.0/sqrt(2.0).

mf(X, Y, f(Y0, Y1)) :- r(R), Y0 is X*R+Y*R, Y1 is X*R-Y*R. 

% This version of applyFilter can take two-element
% filters. This should accomodate quite a few filters,
% like Malat, Haar, Daubeauchies, Ferraz de Mello, etc.
applyFilter(_F, [], []) :- !.
applyFilter(F, [X0, X1|XS], [Filtered|YS]) :-
        Filter =.. [F, X0, X1, Filtered],
	call(Filter),
	applyFilter(F, XS, YS).

% Here is the transform. 
wv(_F, [X], [X]) :- !.
wv(F, XS, YS) :-
    applyFilter(F, XS, FS), 
    unzip(FS, MS, DS),
    wv(F, MS, WMS),
    append(WMS, DS, YS).

% unzip was simplified to the case of 
% two element filters.
unzip([], [], []) :- !.
unzip([f(X, Y)|FS], [X|XS], [Y|YS]) :-
   unzip(FS, XS, YS). 

% Euclidean distance between direct
% and reverse transformation.
% One dimensional case.
compare([], [], S, S) :- !.
compare([X|XS], [Y|YS], S1, S) :-
   Next is (X-Y)*(X-Y)+S1,
   compare(XS, YS, Next, S).

% Imitation of two dimensional transforms.
% True two dimensional transforms is quite
% difficult without arrays (it is very
% inefficient to go through columns in
% lists of lists.
loop(M, M, M) :- !.
loop(I, _M, I).
loop(I, M, J) :- I1 is I+1,
        loop(I1, M, J).

% Fake processing of the transform.
proc([], []) :- !.
proc([X|XS], [Y|YS]) :-
   sign(X, S),
   Y is S*sqrt(X*X),
   proc(XS, YS).

sign(X, 1) :- X>0, !.
sign(X, -1) :- X<0.

% run uses the one dimensional transform
% to simulate bidimensional transformation.
run(N, M, Filter) :-
   asserta(pr([])),
   loop(1, M, J), % goes through y dimension
%   display('Starting'), nl,
   makeExample(1, N, Ex), % create x-line
   wv(Filter, Ex, Res1), % apply transform
   proc(Res1, Res),
   ahaar(Res, Back),
   compare(Ex, Back, 0, Precision),
   retract(pr(P1)), 
%   display([f(J, Precision)|P1]), nl,
   asserta(pr([f(J, Precision)|P1])), fail.
run(_, _, _Filter) :-
  retract(pr(Ps)), write(Ps), nl.

% Create a fake signal.
makeExample(I, I, [I]) :- !.
makeExample(I, J, [I|XS]) :-  I1 is I+1, makeExample(I1, J, XS).	

% Without arrays, it is very difficult
% the implementation of a general back transform.
% Here, I present the particular case of Haar transform.
cf(1, [X|XS], [Y|YS], [N|NS]) :- r(R), N is X*R-R*Y, !,
              cf(0, XS, YS, NS).
cf(0, [X|XS], [Y|YS], [N|NS]) :- r(R), N is X*R+R*Y, !,
              cf(1, [X|XS], [Y|YS], NS).

cf(_, _, _, []).

% Implementation of drop.
drop(_B, [], []) :- !.
drop([], A, A) :- !.
drop([_|B], [_|A], R) :- drop(B, A, R).

% Haar back transform
ahar(This, [], This, []) :- !.
ahar([HB|B], [HA|A], C, D) :-
    r(R),
    X is HB*R+HA*R,
    cf(1, [HB|B], [HA|A], CF),
    drop(B, A, DROPED),
    A1 = [X|CF], 
   ahar(A1, DROPED, C, D).

% Calling Haar back transform
ahaar([X|XS], Res) :-
   ahar([X], XS, Res, _).
   
main :-
        statistics(runtime, _),
        run(65536, 32, mf), 
        nl,
        statistics(runtime, [_,T]),
        write(total_time(T)), nl.

% :- initialization(q).
