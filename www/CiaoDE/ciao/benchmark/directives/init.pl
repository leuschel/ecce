
:- module(init,[p/0]).
:- use_module(builtin).

% p :- q(X), display(X), nl.

p:- display(eres), nl.

%:- initialization(assert(q(a))).
:- initialization((display(hola),nl)).
