
:- module(dyn,[d/1]).

% :- dynamic p/0, q/0, r/0, s/0.
:- dynamic p/1.

p(a).

d(X) :- p(X).
