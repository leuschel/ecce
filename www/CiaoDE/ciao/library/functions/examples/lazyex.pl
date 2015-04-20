:- module(_,_,[functions,lazy]).

:- use_module(library('lazy/lazy_lib'), [tail/2, zipWith/4]).

:- lazy function fiblist/0.

fiblist := [0, 1 | ~zipWith(add, FibL, ~tail(FibL))]
        :- FibL = fiblist.

:- function add/2.
add(X,Y) := X + Y.

