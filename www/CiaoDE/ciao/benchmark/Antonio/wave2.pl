:- module(_,[main/1]).

:- use_module(library(dynamic)).
:- use_module(library(prolog_sys)).
:- use_module(library(lists)).

:- dynamic emg/2.

main([T1, T2]) :- 
        testLearning(J, T1, T2), 
        write(J), 
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
 %% implementation of arrays is necessary.

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

 %% One must substitute real arrays for this piece of code. To preserve
 %% purity, one could use unique types, like in Mercury.  I leave this to
 %% implementors.

% This just retrieves nth elem -- already in library.  Perhaps
% associative arrays should be used for it.
elem(L, I, Elem) :- 
        I1 is I + 1,
        nth(I1, L, Elem).

% Another way of dealing with arrays is through retract and asserts.
% Use indexing to improve execution. 
sawtooth(I, I, _X, _Step) :- !,
        !.
sawtooth(I, J, X, Step) :-
        assertz(emg(I, X)),
        X1 is X+Step,
        I1 is I+1,
        sawtooth(I1, J, X1, Step).

% A fake electromyographic signal, just for test.
signal(sawtooth, NSamples, Step) :- 
        retractall(emg(_, _)),
        sawtooth(0, NSamples, 1.0, Step).


% In the modified Hoare notation for array update,
% the specification for newA should be:
% newA.[I <- a.[I]-2.0*MU*(y.[N]-Yhat)*
%         (if (I>N) then 0.0 else Y.[N-i]) \\
%          I in [0..m]]
% Since the filter is provided by the user, the Prolog program must be
% generated automatically from the specification. This can be easily
% done, if one knows an efficient way to represent arrays in Prolog.
% 
newA(_A, I,  _MU, _Yhat, _N, M, NA) :-
        I > M,
        !,
        NA= [].
newA(A, I, MU, Yhat, N, M, [X|NA]) :-
        elem(A, I, AI), % This certainly can be optimized.
        ( 
            I > N -> 
            X is AI 
        ;
            emg(N,YN),
            Ni is N-I,
            emg(Ni, YNi),
            X is AI-2.0*MU*(YN-Yhat)*YNi
        ),
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

yHATloop(_A,  I, _N, UNTIL, YHAT) :- 
        I>UNTIL, 
        !, 
        YHAT= 0.0.
yHATloop(A, I, N, UNTIL, YHAT) :-
        elem(A, I, Ai),
        Ni is N-I,
        emg(Ni, YNi),
        E is YNi*Ai,
        I1 is I+1,
        yHATloop(A, I1, N, UNTIL, YHAT1),
        YHAT is YHAT1 + E. 


% One step of the learning algorithm.
learningLoop(N, NSamples, _M, _MU, A, NewA, []) :-
        N = NSamples,
        !,
        NewA= A.
learningLoop(N, NSamples, M, MU, A, NewA, [YHAT|YS]) :-
        N1 is N+1,
        yHat(A, N, M, YHAT),
        newA(A, 0, MU, YHAT, N, M, A1),
        learningLoop(N1, NSamples, M, MU, A1, NewA, YS).

featureRetrieving(A, NSamples, MU, f(NewA, YHATvector)) :-
        length(A, LenA),
        M is LenA-1,
        learningLoop(0, NSamples, M, MU, A, NewA, YHATvector).

errorEstimation(_I, [], Es, E) :- !,
        average(Es, 0.0, 0.0, M),
        deviation(Es, M, 0.0, 0.0, E).
errorEstimation(I, [Yh|YhS], Es, E) :-
        emg(I, Y),
        E1 is Y-Yh,
        I1 is I+1,
        errorEstimation(I1, YhS, [E1|Es], E).

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

getFeatures6(J, NSamples, A, MU, Features, Estimation, Iterations) :-
        maxIter(MI), 
        J > MI,
        !, 
        Iterations= J,
        featureRetrieving(A, NSamples, MU, f(Features, Estimation)).
getFeatures6(J, NSamples, A, MU, Features, Estimation, Iterations) :-
        featureRetrieving(A, NSamples, MU, f(NewA, YHATvector)),
        errorEstimation(0, YHATvector, [], EErr),
        minError(MErr), 
        EErr > MErr, !,
        J1 is J+1,
        getFeatures6(J1, NSamples, NewA, MU, Features, Estimation, Iterations).
getFeatures6(J, NSamples, A, MU, Features, Estimation, Iterations) :-
        featureRetrieving(A, NSamples, MU, f(Features, Estimation)),
        Iterations= J.

makeFirstA(0, []) :- !.
makeFirstA(N, [0.0|XS]) :- 
        N > 0,
        N1 is N-1, 
        makeFirstA(N1, XS).
     
getFeatures(NFeatures, NSamples, MU,  Features, Estimation, Iterations) :-
        makeFirstA(NFeatures, A),
        getFeatures6(0, NSamples, A, MU, Features, Estimation, Iterations). 

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
        signal(sawtooth, NSamples, SampleRate),
        statistics(runtime, [_,T1]),
        NFeatures= 4,
        % getFeatures accesses these facts
        getFeatures(NFeatures, NSamples, MU, Features, Estimation, Iterations),
        statistics(runtime, [_,T2]),
        reverse(Estimation, Ans),
        write('Features= '), write(Features), nl,
        write('Estimation= '), write_some(20, Ans), nl.
