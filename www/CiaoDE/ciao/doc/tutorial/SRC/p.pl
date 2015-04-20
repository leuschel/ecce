
:- module(p,[p/1]).
:- use_module(library(lists)).

:- multifile p/1.
p(X):- p:(X:append(A,B,C)).
