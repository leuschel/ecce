:- module(_,[main/0]).

:- use_module(library(dynamic)).
:- use_module(library(prolog_sys)).
:- use_module(library(lists)).
:- use_module(upd_vectors).

%:- dynamic emg/2.

main:- 
        testLearning(J, T1, T2), 
        write(J), 
        nl,
        write([T1, T2]),
        nl,
	flush_output.

 %% The complete algorithm has two steps.  The first stepts try to
 %% retrieve a set of features from a signal.  The second step is an
 %% associative memory, implemented as a neural network (well, the two
 %% steps use neural network algorithms). The neural network of the second
 %% step learn the features retrieved by the first step. Thus, the second
 %% step will be able to recognize these features later on.

 %% The problem in hand is to build a mechanical arm to be used by
 %% amputees. Dr. Soares, the the leader of this project has built a light
 %% arm to (less than 120g) to be used by children.  In order to achieve
 %% such a light weight prothesis, a special material called shape memory
 %% alloy was used as actuator for the joints. If you are interested, you
 %% can find information about this problem in Soares et al., "The
 %% Application os SMA in the Design of Prosthetic Devices", Proceedings
 %% of the Second International Conference on Shape Memory and
 %% Superelastic Technologies.  Electromyographic signals are used to
 %% control the mechanical arm. The signals are collected from what
 %% remained of the lost limb. Although difficult, the problem is not as
 %% difficult as speech recognition, because the amputee can learn to
 %% generate signals that are successful in producing the desired control
 %% movements.

 %% If the electromyographic signal is going to be processed in real time,
 %% one cannot do without a fast and efficient code. Efficient
 %% implementation of vectors is necessary.

 %% The recognition of electromyographic signals is usually performed by
 %% methods such as AR models and neural networks. However, you can see
 %% from Aghili, 1995, that the rate of failure is very high (Aghili and
 %% Haghpanali, Use of Pattern Recognition Technique to Control a
 %% Multifunctional Prothesis, Medical and Biological Engineering and
 %% Computing, Vol. 33, Pag. 504-508).  The reason is that people feed ad
 %% hoc and subjective features to the neural net (like: How many times
 %% the signal crosses the X-axis? How many peaks does it has?) This
 %% program achieves almost 100% hits by using an automatic feature
 %% retrieving algorithm.

 %% One must substitute real vectors for this piece of code. To preserve
 %% purity, one could use unique types, like in Mercury.  I leave this to
 %% implementors.

% This just retrieves nth elem -- already in library.  Perhaps
% associative vectors should be used for it.
elem(L, I, Elem) :- 
        I1 is I + 1,
        nth(I1, L, Elem).

% Another way of dealing with vectors is through retract and asserts.
% Use indexing to improve execution. 
sawtooth(I, I, _X, _Step, _Emg) :- !,
        !.
sawtooth(I, J, X, Step, Emg) :-
        X1 is X+Step,
        I1 is I+1,
        unify_vector_element(Emg, I, X),
        sawtooth(I1, J, X1, Step, Emg).

% A fake electromyographic signal, just for test.
signal(sawtooth, NSamples, Step, Res) :- 
       make_vector_free(NSamples, Res),
       sawtooth(0, NSamples, 1.0, Step, Res).


% In the modified Hoare notation for vector update,
% the specification for newA should be:
% newA.[I <- a.[I]-2.0*MU*(y.[N]-Yhat)*
%         (if (I>N) then 0.0 else Y.[N-i]) \\
%          I in [0..m]]
% Since the filter is provided by the user, the Prolog program must be
% generated automatically from the specification. This can be easily
% done, if one knows an efficient way to represent vectors in Prolog.
% 
newA(_A, I,  _MU, _Yhat, _N, M, NA, _Emg) :-
        I > M,
        !,
        NA= [].
newA(A, I, MU, Yhat, N, M, [X|NA], Emg) :-
        elem(A, I, AI), % This certainly can be optimized.
        ( 
            I > N -> 
            X is AI 
        ;
            access_vector(Emg, N, YN),
            Ni is N-I,
            access_vector(Emg, Ni, YNi),
            X is AI-2.0*MU*(YN-Yhat)*YNi
        ),
        I1 is I+1,
        newA(A, I1, MU,  Yhat, N, M, NA, Emg).

yHat(A, N, M, YHAT, Emg) :- M < N, !,
        UNTIL= M,
        yHATloop(A, 0, N, UNTIL, YHAT1, Emg),
        YHAT is -YHAT1.
yHat(A, N, _M, YHAT, Emg) :- 
        UNTIL = N,
        yHATloop(A, 0, N, UNTIL,  YHAT1, Emg),
        YHAT is -YHAT1.

yHATloop(_A,  I, _N, UNTIL, YHAT, _Emg) :- 
        I>UNTIL, 
        !, 
        YHAT= 0.0.
yHATloop(A, I, N, UNTIL, YHAT, Emg) :-
        elem(A, I, Ai),
        Ni is N-I,
%        Ni_1 is Ni + 1,
        access_vector(Emg, Ni, YNi),
%        aref(Ni, Emg, YNi),
%        emg(Ni, YNi),
        E is YNi*Ai,
        I1 is I+1,
        yHATloop(A, I1, N, UNTIL, YHAT1, Emg),
        YHAT is YHAT1 + E. 


% One step of the learning algorithm.
learningLoop(N, NSamples, _M, _MU, A, NewA, [], _Emg) :-
        N = NSamples,
        !,
        NewA= A.
learningLoop(N, NSamples, M, MU, A, NewA, [YHAT|YS], Emg) :-
        N1 is N+1,
        yHat(A, N, M, YHAT, Emg),
        newA(A, 0, MU, YHAT, N, M, A1, Emg),
        learningLoop(N1, NSamples, M, MU, A1, NewA, YS, Emg).

featureRetrieving(A, NSamples, MU, f(NewA, YHATvector), Emg) :-
        length(A, LenA),
        M is LenA-1,
        learningLoop(0, NSamples, M, MU, A, NewA, YHATvector, Emg).

errorEstimation(_I, [], Es, E, _Emg) :- !,
        average(Es, 0.0, 0.0, M),
        deviation(Es, M, 0.0, 0.0, E).
errorEstimation(I, [Yh|YhS], Es, E, Emg) :-
%        I_1 is I + 1,
        access_vector(Emg, I, Y),
%        aref(I, Emg, Y),
%        emg(I, Y),
        E1 is Y-Yh,
        I1 is I+1,
        errorEstimation(I1, YhS, [E1|Es], E, Emg).

average([], S, L, Avg) :- Avg is S/L.
average([X|XS], S, L, Avg) :-
        S1 is X+S,
        L1 is 1+L,
        average(XS, S1, L1, Avg).

deviation([], _M, S, L, E) :- E is S/L.
deviation([X|XS], M, S, L, E) :-
        S1 is (X-M)*(X-M)+S,
        L1 is 1+L,
        deviation(XS, M, S1, L1, E).

maxIter(100).
minError(0.01).

getFeatures6(J, NSamples, A, MU, Features, Estimation, Iterations, Emg) :-
        maxIter(MI), 
        J > MI,
        !, 
        Iterations= J,
        featureRetrieving(A, NSamples, MU, f(Features, Estimation), Emg).
getFeatures6(J, NSamples, A, MU, Features, Estimation, Iterations, Emg) :-
        featureRetrieving(A, NSamples, MU, f(NewA, YHATvector), Emg),
        errorEstimation(0, YHATvector, [], EErr, Emg),
        minError(MErr), 
        EErr > MErr, !,
        J1 is J+1,
        getFeatures6(J1, NSamples, NewA, MU, Features, Estimation, Iterations, Emg).
getFeatures6(J, NSamples, A, MU, Features, Estimation, Iterations, Emg) :-
        featureRetrieving(A, NSamples, MU, f(Features, Estimation), Emg),
        Iterations= J.

makeFirstA(0, []) :- !.
makeFirstA(N, [0.0|XS]) :- 
        N > 0,
        N1 is N-1, 
        makeFirstA(N1, XS).
     
getFeatures(NFeatures, NSamples, MU,  Features, Estimation, Iterations, Emg) :-
        makeFirstA(NFeatures, A),
        getFeatures6(0, NSamples, A, MU, Features, Estimation, Iterations, Emg). 

write_some(0, _XS) :- !.
write_some(_I, []) :- !.
write_some(I, [X|XS]) :-
        write(X), write(', '),
        I1 is I-1,
        write_some(I1, XS).

testLearning(Iterations, T1, T2) :-
        MU is 0.01,
        SampleRate = 0.0001,
        NSamples = 20000,
        % signal/3 returns all data in a collection of fact
        statistics(runtime, _),
        signal(sawtooth, NSamples, SampleRate, Emg),
        statistics(runtime, [_,T1]),
        NFeatures= 4,
        % getFeatures accesses these facts
        getFeatures(NFeatures, NSamples, MU, Features, Estimation, Iterations,  Emg),
        statistics(runtime, [_,T2]),
        reverse(Estimation, Ans),
        write('Features= '), write(Features), nl,
        write('Estimation= '), write_some(20, Ans), nl.
