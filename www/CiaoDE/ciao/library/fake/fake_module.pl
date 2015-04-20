:- module(_, [main/0], []).

:- ensure_loaded(library(class)).

%% Force creation of class.itf and class.asr by having this fake module 
%% which uses it.

main.
