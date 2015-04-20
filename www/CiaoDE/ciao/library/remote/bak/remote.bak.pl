:- use_module(library('remote/ciao_client')).
:- use_module(library('remote/ciao_server')).
:- include(library('remote/ops')).
:- include(library(hlc)).

:- multifile call_in/1.
call_in(X):- call(X).
