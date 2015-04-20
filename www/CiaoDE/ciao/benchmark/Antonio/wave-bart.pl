:- use_module(library(prolog_sys)).


applyFilter(_F, [], []) :- !.
applyFilter(F, [X0, X1|XS], [Filtered|YS]) :-
        callfilter(F,X0, X1, Filtered),
%%         Filter =.. [F, X0, X1, Filtered],
        %% call(Filter),
 applyFilter(F, XS, YS).

callfilter(mf,X0, X1, Filtered) :- !,
        mf(X0, X1, Filtered).
callfilter(asd,_X0, _X1, _Filtered).

applyFilter2(mf,A,B) :- !, applyFiltermf(A,B).
applyFilter2(asd,_,_).

applyFiltermf([],[]).
applyFiltermf([X0, X1|XS], [Filtered|YS]) :-
                                                % mf(X0, X1, Filtered),
 Filtered = f(Y0, Y1),
 R is 1.0/sqrt(2.0),
 Y0 is X0*R+X1*R, Y1 is X0*R-X1*R,
 applyFiltermf(XS, YS).

applyFilter3(mf,A,B,C) :- !, applyFiltermf(A,B,C).
applyFilter3(asd,_,_).

applyFiltermf([],[],[]).
applyFiltermf([X0, X1|XS], [Y0|YS],[Y1|Zs]) :-
        R is 1.0/sqrt(2.0),
        Y0 is X0*R+X1*R,
        Y1 is X0*R-X1*R,
        applyFiltermf(XS, YS, Zs).



% Here is the transform.
wv(_F, [X], [X]) :- !.
wv(F, XS, YS) :-
%     applyFilter2(F, XS, FS),
    applyFilter3(F, XS, MS, DS),
%     applyFilter(F, XS, FS),
%     unzip(FS, MS, DS),
    wv(F, MS, WMS),
    append(WMS, DS, YS).

% run uses the one dimensional transform
% to simulate bidimensional transformation.
run(N, M, Filter) :-
   loop(1, M, J), % goes through y dimension
   makeExample(1, N, Ex), % create x-line
   wv(Filter, Ex, Res1), % apply transform
   proc(Res1, Res),
   ahaar(Res, Back),
   compare(Ex, Back, 0, _Precision),
   write(ok(J)),nl,
   fail.
run(_, _, _Filter).


main :-
        statistics(runtime, _),
        run(65536, 50, mf), 
        nl,
        statistics(runtime, [_,T]),
        write(total_time(T)), nl,
        statistics.

r(X) :- X is 1.0/sqrt(2.0).

mf(X, Y, f(Y0, Y1)) :- r(R), Y0 is X*R+Y*R, Y1 is X*R-Y*R. 

% unzip was simplified to the case of 
% two element filters.
unzip([], [], []) :- !.
unzip([f(X, Y)|FS], [X|XS], [Y|YS]) :-
   unzip(FS, XS, YS). 

% Euclidean distance between direct
% and reverse transformation.
% One dimensional case.
compare([], [], S, S).
compare([X|XS], [Y|YS], S1, S) :-
   Next is (X-Y) * (X-Y) + S1,
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
proc([], []).
proc([X|XS], [Y|YS]) :-
%    sign(X, S),
        (X > 0 ->
             S = 1
        ;
                S = -1
        ),
        Y is S*sqrt(X*X),
        proc(XS, YS).

sign(X, 1) :- X>0, !.
sign(X, -1) :- X<0.


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

cf1( [X|XS], [Y|YS], [N|NS]) :- r(R), N is X*R-R*Y, !,
              cf0( XS, YS, NS).
cf1( _, _, []).

cf0( [X|XS], [Y|YS], [N|NS]) :- r(R), N is X*R+R*Y, !,
              cf1( [X|XS], [Y|YS], NS).

cf0( _, _, []).

% Implementation of drop.
%% drop(_B, [], []) :- !.
%% drop([], A, A) :- !.
%% drop([_|B], [_|A], R) :- drop(B, A, R).

drop([],L,L).
drop([_|R],X,Res) :-
        drop2(X,R,Res).

drop2([],_,[]).
drop2([_|S],R,Res) :-
        drop(R,S,Res).


%% % Haar back transform
ahar(This, [], This, []) :- !.
ahar([HB|B], [HA|A], C, D) :-
        r(R),
        X is HB*R+HA*R,
        cf(1, [HB|B], [HA|A], CF),
        drop(B, A, DROPED),
        A1 = [X|CF], 
        ahar(A1, DROPED, C, D).

%% % Calling Haar back transform
ahaar([X|XS], Res) :-
%    ahar([X], XS,  Res, _).
   ahar2(XS, [X], Res, _).

ahar2([], This, This, []) :- !.
ahar2( [HA|A], [HB|B], C, D) :-
        r(R),
        X is HB*R+HA*R,
        cf1( [HB|B], [HA|A], CF),
        drop(B, A, DROPED),
        A1 = [X|CF],
        ahar2(DROPED, A1, C, D).

append([],L,L).
append([X|R],S,[X|T]) :- append(R,S,T).
