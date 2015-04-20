
% This file makes sicstus shell compatible with CIAO

% For now, only for xrefs/davinci libraries ...

%% :- dynamic library_directory/1.

:- asserta(
   library_directory('/home/clip/Systems/ciao/library/xrefs') ).
:- asserta(
   library_directory('/home/clip/Systems/ciao/library/davinci') ).
:- asserta(
   library_directory('/home/clip/Systems/ciao/library') ).
:- asserta(
   library_directory('/home/clip/Systems/ciao/lib') ).
:- asserta(
   library_directory('/home/clip/Systems/ciao/compatibility/sicstus3') ).

:- consult(user:library(user)).
:- consult(library(builtin)).
