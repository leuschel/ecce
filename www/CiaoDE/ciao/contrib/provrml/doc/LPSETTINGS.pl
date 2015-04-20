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
% include ../../SETTINGS
%% ignored line: %% include ../../../doc/SETTINGS.COMMON
% ----------------------------------------------------------------------------
% For using the development version of ciaodoc when testing...
% LIBDIR     = /home/clip/Systems/ciaodoc/lib
% CIAODOC    = /home/clip/Systems/ciaodoc/src/ciaodoc
% ----------------------------------------------------------------------------
% FILEPATHS := $(CIAOSRC)/doc/reference $(CIAOSRC)/shell $(CIAOSRC)/ciaoc $(CIAOSRC)/engine \
%              $(CIAOSRC)/emacs $(CIAOSRC)/etc \
% 	     $(FILEPATHS)
filepath := '..'.
% Temporary...
%MAINOPTS  := -v $(MAINOPTS)
%COMPOPTS  := -v $(COMPOPTS)
mainfile := 'vrml'.
% The order is very important!
% COMPONENTS = $(REFCOMPONENTS) $(LIBCOMPONENTS) \
%              $(LIBRARYCOMPONENTS) $(UTILCOMPONENTS)
% COMPONENTS = $(REFCOMPONENTS) $(LIBCOMPONENTS) \
%               $(LIBRARYCOMPONENTS) $(UTILCOMPONENTS)
component := ~maincomponents.
maincomponents := 'ProVRML_intro'|
    'boundary'|
    'check'|
    'dictionary'|
    'dictionary_tree'|
    'error'|
    'field_type'|
    'field_value'|
    'field_value_check'|
    'generator'|
    'generator_util'|
    'internal_types'|
    'io'|
    'lookup'|
    'parser'|
    'parser_util'|
    'possible'|
    'tokeniser'|
    'vrml'.
