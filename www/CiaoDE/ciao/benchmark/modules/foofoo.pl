
:- module(foofoo,[p/1,p1/1]).
%:- use_module(length,[length/2]). % file and module names different
%:- use_module(utilities,[length/2]). % ok
:- compile(utilities).


p(N) :- q(X), length(X,N).

p1(N):- q(X), length1(X,0,N).

q([1,2,3,4]).
