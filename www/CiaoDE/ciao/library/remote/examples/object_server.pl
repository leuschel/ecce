:- module(_, [main/0], []).

:- use_package(remote).
:- use_package(objects).
:- use_class(library('remote/examples/stack')).
:- use_module(library(format)).
:- use_module(library(aggregates)).
:- use_module(engine(internals), [initialization/1]).


p(a).
p(b).
q(aa).
q(bb).

%r :- X is 2+2.
	

main:- serve.
