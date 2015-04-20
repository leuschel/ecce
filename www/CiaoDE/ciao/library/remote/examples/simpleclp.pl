:- module(simpleclp, [c/2]).

:- use_package(clpq).

c(X, Y):- X .>. Y.
