:- module(fib_function, _, [hiord, functions, lazy]).

:- use_module(library(lists), [nth/3]).
:- use_module(library('lazy/lazy_lib'), [tail/2, zipWith/4]).


:- function add/2.
add(X,Y) := X + Y.

:- lazy function fib/0.
fib := [0, 1 | ~zipWith(add, L, ~tail(L))]
    :- fib(L).

:- function main/1.
main(Element) := ~nth(Element, fib).
