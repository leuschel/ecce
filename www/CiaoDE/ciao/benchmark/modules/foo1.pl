
:- module(foo1,[p/1,q/1]).
:- use_module(utilities,[length/2]).
:- use_module(user,[mycall/2]).

p(N) :- mycall(q,[X]), length(X,N).

q([1,2,3,4]).
