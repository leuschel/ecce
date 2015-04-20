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
%% ignored line: %% include ../../../SETTINGS
%% ignored line: %% include ../../../doc/SETTINGS.COMMON
% ----------------------------------------------------------------------------
filepath := ~atom_concat( [ ~ciaosrc , '/library.development/plspeed' ] )|
    ~atom_concat( [ ~ciaosrc , '/library.development/plspeed/doc' ] ) | ~filepath.
mainfile := 'benchmarking'.
fileoption(~mainfile) := '-norefs'.
