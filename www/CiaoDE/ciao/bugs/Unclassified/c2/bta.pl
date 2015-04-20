

:- module('bta', [ a/1 ]).
:- use_module('pp').
:- use_module('cogen-tools').
:- use_module(library(write)).


a(X) :- b(X), print('found :'),print(X), nl.


:- initialization(a(X)).