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
%% ignored line: %% include /home/clip/lib/lpdoc/SETTINGS
% ----------------------------------------------------------------------------
%ciaosrc := ~ciaosrc.
filepath := ~atom_concat( [ ~ciaosrc , '/library/assertions' ] ) | ~atom_concat( [ ~ciaosrc , '/doc/common' ] )|
    ~atom_concat( [ ~ciaosrc , '/library/regtypes' ] ).
systempath := ~atom_concat( [ ~ciaosrc , '/lib' ] ) | ~atom_concat( [ ~ciaosrc , '/library' ] ) | ~atom_concat( [ ~ciaosrc , '/library/assertions' ] ).
mainfile := 'assrt_doc'.
% Optional...
fileoption(~mainfile) := '-v'.
fileoption(~component) := '-v'.
docformat := 'dvi' | 'info' | 'infoindex' | 'htmlindex'.
component := 'assrt_props'.
%	regtypes_tr.pl
%	assrt_lib.pl
%	assrt_write.pl
