:- module(module2, [squares/2], [functions, lazy]).

:- use_module(library('lazy/lazy_lib'), [lazy_map/3]).


square(X) := X * X.

:- lazy function squares/1.
squares(List) := ~lazy_map(List, (''(X, Y) :- Y is X * X)).

