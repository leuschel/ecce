
%% Definitions for CHIP programs to run on CIAO

:- include(library(pure)).        % so that no ciao libs are included
:- include(engine(basic_props)).  % so as not to complain about basic props

:- include(library(assertions)).       % assertions
:- include(library(regtypes)).         % types
% this is directly done in chipre
%:- include(library('chip/chip_ops')).
:- include(library('chip/chip_decl')). % chip declarations

:- load_compilation_module(library('chip/chip_exp')).
:- add_sentence_trans(expand_chip/2).
