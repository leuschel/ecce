
:- module(p,[p/1]).
:- use_module(library(modules)).

:- dynamic p/1, s/1.
:- dynamic r/1.
:- dynamic o.

p(b).

:- include(q).
