:- module(fib, [do_fib/0,fib/2],[]).

:- use_module(library(prolog_sys)).
:- use_module(library(format)).


n(0).
n(X):-
        n(Y),
        X is Y + 1.

fib(N,F):- 
        n(N),
        fibaux(0,1,N,F).

fibaux(Fact, _Fpost, 0,  Fact).
fibaux(Fact, Fpost, N, F):-
	N > 0,
        N1 is N - 1,
        Nfib is Fact + Fpost,
	fibaux(Fpost, Nfib, N1, F).


do_fib:-
        statistics(runtime, _),
        N = 1800,
        fib(N, F),
        fib(_K, F),
        statistics(runtime, [_|T]),
        display('Fibonacci: '),
        display(T),
        display(' milliseconds'),
        nl.
