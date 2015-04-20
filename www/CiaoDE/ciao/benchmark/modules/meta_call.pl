
:- module(m,[c/1]).
:- use_module(builtin).
:- use_module(meta,[mycall/1]).

c(N) :- meta:mycall(q(X)), length(X,N).

q([1,2,3,4]).
