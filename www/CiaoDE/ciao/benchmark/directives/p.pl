
:- module(p,[]).
%:- use_module(m).
:- use_module(builtin).

:- op( 400, xfy,p). 

A p B :- display(ok), nl.
