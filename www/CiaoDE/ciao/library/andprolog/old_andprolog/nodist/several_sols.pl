:- module(several_sols, [main/2], []).


:- use_module(library(andprolog)).


:- reexport(library(andprolog),[active_agents/1, &/2]).


main(X, Y):- a(X) & b(Y).


a(1).
a(2).
a(3).


b(a).
b(b).
b(c).
