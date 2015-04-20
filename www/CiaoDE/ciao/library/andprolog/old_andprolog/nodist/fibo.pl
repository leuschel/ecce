:- module(fibo, [
        mktests/3,
        test/2,
        fibo/3],
        []).

:- use_module(library(andprolog)).

:- reexport(library(andprolog),[active_agents/1, (&)/2]).


:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format), [format/2]).


fibo(0, 0, _).
fibo(1, 1, _).
fibo(N, F, Level):-
        N > 1,
        (
            N < Level ->
            fibo_seq(N, F)
        ;
            N1 is N - 1,
            N2 is N - 2,
            fibo(N1, F1, Level) & fibo(N2, F2, Level),
            F is F1 + F2
        ).

fibo_seq(0, 0).
fibo_seq(1, 1).
fibo_seq(N, F):-
        N > 1,
        N1 is N - 1,
        N2 is N - 2,
        fibo_seq(N1, F1),
        fibo_seq(N2, F2),
        F is F1 + F2.

test(Level, Time):-
        N = 24,
        statistics(walltime, _),
        fibo(N, F, Level),
        statistics(walltime, [_,Time]),
        (
            F = 46368 ->
            true
        ;
            format("Error in fibonacci(~d)~n", [N]),
            Time = 0
        ).


%% Perform N tests at Level (using the current number of agents)
%% Throw away the best and worst times, and average the rest.

mktests(N, Level, Average):-
        mktests_l(N, Level, TimeList),
        TimeList = [T|_],
        sumlist(TimeList, T, T, Best, Worst, Sum),
        Average is (Sum - Best - Worst) / (N-2).

sumlist([], B, W, B, W, 0).
sumlist([T|Ts], CurrB, CurrW, B, W, Sum):-
        (T > CurrB -> BNow = T; BNow = CurrB),
        (T < CurrW -> WNow = T; WNow = CurrW),
        sumlist(Ts, BNow, WNow, B, W, SPar),
        Sum is SPar + T.

mktests_l(0, _Level, []).
mktests_l(N, Level, [T|Ts]):-
        N > 0,
        test(Level, T),
%%        format("Time: ~d miliseconds~n", [T]),
        N1 is N - 1,
        mktests_l(N1, Level, Ts).


%% Ejemplo de pruebas: 
%%
%% between(L, 18, 26), mktests(10, L, T), display(T), nl, fail.
%% 
%% Cambiar el numero de agentes y repetir.
%%
%% Version: ciao-0.9 patch 6.
%% Compilation: nodebug, threads, locks from Alan@xorguk, -O3
%% Machine: orion.ctp.fi.upm.es, SPARCcenter-2000
%% S.O.: Solaris 5.5
%% Other: alone in the machine

%% Using non deterministic apc2
%% Level:    18    19    20    21    22    23    24    25    26
%%\\\\\
%% Agents:
%% 1       1555  1530  1549  1541  1582  1584  1613  1325  1324
%% 2        837   831   837   858   967   999  1029  1332
%% 3        570   570   588   611   746   635  1062  1332
%% 4        453   466   486   477   596   620  1012
%% 5        363   380   392   453   411   617  1029
%% 6        313   331   347   384   394   632  1030
%% 7        290   266   299   378   393   628  1081
%% 8        256   252   289   252   396   658  1025
%% 9        229   247   256   262   403   633  1040

%% Using  deterministic apcdet
%% Level:    18    19    20    21    22    23    24    25    26
%%\\\\\
%% Agents:
%% 1       1461
%% 2        775
%% 3        516
%% 4        409
%% 5        325
%% 6        281
%% 7        242
%% 8        219
%% 9        191
