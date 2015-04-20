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
docformat := 'ps'.
index := 'concept' | 'pred' | 'prop' | 'regtype' | 'usage'.
% ----------------------------------------------------------------------------
% Uncomment for using the development version of lpdoc when testing...
% LIBDIR     = /home/clip/Systems/lpdoc/lib
% LPDOC      = /home/clip/Systems/lpdoc/src/lpdoc
% ----------------------------------------------------------------------------

filepath := ~atom_concat( [ ~ciaosrc , '/library/javall' ] ).
systempath := ~atom_concat( [ ~ciaosrc , '/lib' ] )|
    ~atom_concat( [ ~ciaosrc , '/library' ] ).
mainfile := 'javall_doc'.
% ******** Warning: if you change this you also need to change 
% ******** ciao.el which gives an outline of the manual
component := 'javart'|
    'jtopl'|
    'javasock'.
% ----------------------------------------------------------------------------
% Select lpdoc options for main file (do lpdoc -h to get a list of options)
% Leaving this blank produces most verbose manuals
% -v -nobugs -noauthors -noversion -nochangelog -nopatches -modes 
% -headprops -literalprops -nopropnames -noundefined -nopropsepln -norefs 
% -nobullet -nosysmods -noengmods -noisoline -propmods -onesided
% 
%MAINOPTS  := -v $(MAINOPTS) # Useful for debugging...
fileoption(~mainfile) := '-nochangelog'.
% ----------------------------------------------------------------------------
% Select lpdoc opts for component file(s) (do lpdoc -h to get list of opts)
% Leaving this blank produces most verbose manuals
% 
%COMPOPTS  := -v $(COMPOPTS) # Useful for debugging...
fileoption(~component) := '-nochangelog' | '-modes'.
