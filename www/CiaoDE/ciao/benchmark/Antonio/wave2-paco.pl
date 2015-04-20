:- module(_,[main/1]).
:- dynamic emg/2.
:- dynamic nSamples/1.
:- dynamic sampleRate/1.

:- use_module(library(prolog_sys)).

main([T1,T2]) :- 
        testLearning(J, T1, T2),
        write(J), 
        nl,
        flush_output.
	

% I do not claim that this program 
% snippet is a good Prolog implementation
% of Paschoarelli's feature retrieving algorithm.
% I wrote it only to reason about
% the features that Prolog must have
% to deal with similar problems.

% The complete algorithm has two steps.
% The first stepts try to retrieve a
% set of features from a signal.
% The second step is an associative
% memory, implemented as a neural network
% (well, the two steps use neural network
% algorithms). The neural network of the
% second step learn the features retrieved
% by the first step. Thus, the second step
% will be able to recognize these features later on. 

% The problem in hand is to build a mechanical
% arm to be used by amputees. Dr. Soares, the
% the leader of this project has built a light
% arm to (less than 120g) to be used by children.
% In order to achieve such a light weight prothesis, 
% a special material called shape memory alloy was
% used as actuator for the joints. If you are
% interested, you can find information about
% this problem in Soares et al., "The Application
% os SMA in the Design of Prosthetic Devices",
% Proceedings of the Second International Conference
% on Shape Memory and Superelastic Technologies.
% Electromyographic signals are used to control
% the mechanical arm. The signals are collected
% from what remained of the lost limb. Although
% difficult, the problem is not as difficult as
% speech recognition, because the amputee can learn
% to generate signals that are successful in
% producing the desired control movements.

% If the electromyographic signal is going to be
% processed in real time, one cannot do without
% a fast and efficient code. Efficient implementation
% of arrays is necessary. 

% The recognition of electromyographic signals
% is usually performed by methods such as AR
% models and neural networks. However, you can
% see from Aghili, 1995, that the rate of failure
% is very high (Aghili and Haghpanali, Use of 
% Pattern Recognition Technique to Control a
% Multifunctional Prothesis, Medical and Biological
% Engineering and Computing, Vol. 33, Pag. 504-508).
% The reason is that people feed ad hoc and subjective
% features to the neural net (like: How many times 
% the signal crosses the X-axis? How many peaks does 
% it has?) This program achieves almost 100% hits by
% using an  automatic feature retrieving algorithm.

% One must substitute real arrays for this
% piece of code. To preserve purity, one
% could use unique types, like in Mercury.
% I leave this to implementors.
elem(I, J, [Elem|_], Elem) :- I=J, !.
elem(I, J, [_|L], Elem) :-
  I1 is I+1, elem(I1, J, L, Elem).

elem(L, I, Elem) :- elem(0, I, L, Elem). 

% Another way of dealing with arrays is
% through retract and asserts.
sawtooth(I, _X, _Step) :- nSamples(J), J=I, !.
sawtooth(I, X, Step) :-
   assertz(emg(X, I)),
   X1 is X+Step,
   I1 is I+1,
   sawtooth(I1, X1, Step).

% A fake electromyographic signal, just for test.
signal(sawtooth, NSamples) :- 
  retractall(emg(_, _)),
  retractall(nSamples(_)),
  assertz(nSamples(NSamples)),
  sampleRate(Step),
  sawtooth(0, 1.0, Step).

% In the modified Hoare notation for array update,
% the specification for newA should be:
% newA.[I <- a.[I]-2.0*MU*(y.[N]-Yhat)*
%         (if (I>N) then 0.0 else Y.[N-i]) \\
%          I in [0..m]]
% Since the filter is provided by the user,
% the Prolog program must be generated automatically
% from the specification. This can be easily done, if
% one knows an efficient way to represent arrays
% in Prolog.
newA(_A, I,  _MU, _Yhat, _N, M, NA) :-
   I > M, !, NA= [].
newA(A, I, MU, Yhat, N, M, [X|NA]) :-
    elem(A, I, AI), % This certainly can be optimized.
   ( I > N -> X is AI ;
     emg(YN,N),
     Ni is N-I,
     emg(YNi, Ni),
     X is AI-2.0*MU*(YN-Yhat)*YNi),
    I1 is I+1,
    newA(A, I1, MU,  Yhat, N, M, NA).

yHat(A, N, M, YHAT) :- M < N, !,
   UNTIL= M,
   yHATloop(A, 0, N, UNTIL, YHAT1),
   YHAT is -YHAT1.
yHat(A, N, _M, YHAT) :- 
   UNTIL = N,
   yHATloop(A, 0, N, UNTIL,  YHAT1),
   YHAT is -YHAT1.

yHATloop(_A,  I, _N, UNTIL, YHAT) :- I>UNTIL, !, YHAT= 0.0.
yHATloop(A, I, N, UNTIL, YHAT) :-
    elem(A, I, Ai),
    Ni is N-I,
    emg(YNi, Ni),
    E is YNi*Ai,
    I1 is I+1,
    yHATloop(A, I1, N, UNTIL, YHAT1),
    YHAT is YHAT1+E. 

test_yHat(Expected, YouGot) :-
   A= [-0.128147, -0.0928003, -0.0586378, -0.0271271],
   retractall(sampleRate(_)),
   assertz(sampleRate(0.2)),
   signal(sawtooth, 20),
   N is  4,
   yHat(A, N, 3, YouGot),
   Expected= 0.4937902.

% A naive test to check whether everything is OK.
test_newA(Expected, NA) :- 
   A = [0, 1, 2, 3],
   MU is 0.01,
   retractall(sampleRate(_)),
   assertz(sampleRate(0.2)),
   signal(sawtooth, 20),
   Yhat= 0.3,
   newA(A, 0, MU, Yhat, 2, 3, NA),
   Expected= [-0.0308, 0.9736, 1.978, 3].

% One step of the learning algorithm.
learningLoop(N, _M, _MU, A, NewA, []) :-
    nSamples(N), !, NewA= A.
learningLoop(N, M, MU, A, NewA, [YHAT|YS]) :-
   N1 is N+1,
   yHat(A, N, M, YHAT),
   newA(A, 0, MU, YHAT, N, M, A1),
   learningLoop(N1, M, MU, A1, NewA, YS).

length([], 0) :- !.
length([_|XS], N) :-
  length(XS, N1),
  N is N1+1.
  
featureRetrieving(A, MU, f(NewA, YHATvector)) :-
   length(A, LenA),
   M is LenA-1,
   learningLoop(0, M, MU, A, NewA, YHATvector).

test_feature(F) :-
   A = [0, 0, 0, 0],
   MU is 0.01,
   retractall(sampleRate(_)),
   assertz(sampleRate(0.2)),
   signal(sawtooth, 20),
   featureRetrieving(A, MU, F).

errorEstimation(_I, [], Es, E) :- !,
  average(Es, 0.0, 0.0, M),
  deviation(Es, M, 0.0, 0.0, E).
errorEstimation(I, [Yh|YhS], Es, E) :-
   emg(Y, I),
   E1 is Y-Yh,
   I1 is I+1,
   errorEstimation(I1, YhS, [E1|Es], E).

average([], S, L, Avg) :- !, Avg is S/L.
average([X|XS], S, L, Avg) :-
   S1 is X+S,
   L1 is 1+L,
   average(XS, S1, L1, Avg).

deviation([], _M, S, L, E) :- !, E is S/L.
deviation([X|XS], M, S, L, E) :-
   S1 is (X-M)*(X-M)+S,
   L1 is 1+L,
   deviation(XS, M, S1, L1, E).

maxIter(100).
minError(0.01).

getFeatures(J, A, MU, Features, Estimation, Iterations) :-
      maxIter(MI), J>MI, !, Iterations= J,
      featureRetrieving(A, MU, f(Features, Estimation)).
getFeatures(J, A, MU, Features, Estimation, Iterations) :-
      featureRetrieving(A, MU, f(NewA, YHATvector)),
      errorEstimation(0, YHATvector, [], EErr),
      minError(MErr), 
      EErr > MErr, !,
      J1 is J+1,
      getFeatures(J1, NewA, MU, Features, Estimation, 
                        Iterations).
getFeatures(J, A, MU, Features, Estimation, Iterations) :-
     featureRetrieving(A, MU, f(Features, Estimation)),
     Iterations= J.

makeFirstA(0, []) :- !.
makeFirstA(N, [0.0|XS]) :- N1 is N-1, makeFirstA(N1, XS).
     
getFeatures(NFeatures, MU,  Features, Estimation, Iterations) :-
   makeFirstA(NFeatures, A),
   getFeatures(0, A, MU, Features, Estimation, Iterations). 

write_some(0, _XS) :- !.
write_some(_I, []) :- !.
write_some(I, [X|XS]) :-
   write(X), write(', '),
   I1 is I-1,
   write_some(I1, XS).

reverse([], R, R) :- !.
reverse([X|XS], R, Rev) :-
  reverse(XS, [X|R], Rev).
  
testLearning(Iterations, T1, T2) :-
   MU is 0.01,
   retractall(sampleRate(_)),
   assertz(sampleRate(0.0001)),
   statistics(runtime, _),
   signal(sawtooth, 20000),
   statistics(runtime, [_,T1]),
   NFeatures= 4,
   getFeatures(NFeatures, MU, Features, Estimation, Iterations), 
   statistics(runtime, [_,T2]),
   reverse(Estimation, [], Ans),
   write('Features= '), write(Features), nl,
   write('Estimation= '), write_some(20, Ans), nl.

% q :- testLearning(J), write(J), nl.

% :- initialization(q).
