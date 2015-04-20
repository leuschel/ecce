:- module(_, _, [functions]).
:- use_module(library(terms)).
:- use_module(ciaosrc('CIAOSETTINGS')).
:- use_module(ciaosrc('CIAOSHARED')).

:- reexport(ciaosrc('doc/common/LPDOCCOMMON')).

:- reexport(ciaosrc('CIAOSETTINGS'), [lpdoclib/1]).

:- redefining(_).

:- discontiguous fileoption/2.

% -*- mode: Makefile; -*-
% ----------------------------------------------------------------------------
%% ignored line: %% include ../../SETTINGS
%% ignored line: %% include ../SETTINGS.COMMON
% ----------------------------------------------------------------------------
% For using the development version of autodoc when testing...
% LIBDIR     = /home/clip/Systems/autodoc/lib
% LPDOC      = /home/clip/Systems/autodoc/src/autodoc
% ----------------------------------------------------------------------------
filepath := ~atom_concat( [ ~ciaosrc , '/doc/compiler' ] ) | ~atom_concat( [ ~ciaosrc , '/ciaoc' ] ) | ~atom_concat( [ ~ciaosrc , '/lib/compiler' ] ) | ~filepath.
% Temporary...
fileoption(~mainfile) := '-v' | ~fileoption(~mainfile).
fileoption(~component) := '-v' | ~fileoption(~component).
mainfile := 'ciao-compiler'.
component := 'ciaoc'|
    'compiler'|
    'exemaker'|
    'c_itf'.
%            callback.pl
