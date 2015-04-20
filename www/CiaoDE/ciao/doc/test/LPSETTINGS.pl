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
%% ignored line: %% include ../../SHARED
%% ignored line: %% include ../SETTINGS.COMMON
% ----------------------------------------------------------------------------
% Uncomment for using the development version of lpdoc when testing...
% LIBDIR     = /home/clip/Systems/lpdoc/lib
% LPDOC      = /home/clip/Systems/lpdoc/src/lpdoc
% ----------------------------------------------------------------------------
filepath := ~atom_concat( [ ~ciaosrc , '/doc/test' ] ) | ~atom_concat( [ ~ciaosrc , '/shell' ] ) | ~atom_concat( [ ~ciaosrc , '/ciaoc' ] ) | ~atom_concat( [ ~ciaosrc , '/engine' ] )|
    ~atom_concat( [ ~ciaosrc , '/emacs-mode' ] ) | ~atom_concat( [ ~ciaosrc , '/etc' ] )|
    ~filepath.
% Useful for debugging...
%MAINOPTS  := -v $(MAINOPTS)
%COMPOPTS  := -v $(COMPOPTS)
mainfile := 'ciao'.
% Mini manual for testing. The order is very important!
%COMPONENTS = $(INSTALLATION) 
% The order is very important!
component := ~getstarted|
    'Append'|
    ~installation.
%    DevEnv.pl \
%	$(DEVENV) \
getstarted := 'GetStartUnix'|
    'GetStartWin32'.
installation := 'Install'|
    'InstallWin32bin'|
    'BeyondInstall'.
devenv := 'ciaoc'|
    'ciaosh_doc'|
    'debugger_doc'|
    'debugger'|
    'ciao-shell'|
    'libpaths'|
    'CiaoMode'.
%	toplev.pl \
refcomponents := 'modules'|
    'loading_code'|
    'basiccontrol'|
    'builtin_directives'|
    'basic_props'|
    'term_typing'|
    'term_basic'|
    'term_compare'|
    'atomic_basic'|
    'arithmetic'|
    'io_basic'|
    'streams_basic'|
    'exceptions'|
    'data_facts'|
    'prolog_flags'|
    'concurrency'|
    'syntax_extensions'|
    'io_aux'|
    'attributes'|
    'system_info'.
%              prolog.src \
%              examples.src 
% Should not be used, so we do not document them
% 	internals.pl \
% Other
% 	mexpand.pl \
% Taken out
%	ciaoengine.pl
isoprolog := 'iso_doc'|
    'operators'|
    'aggregates'|
    'dynamic'|
    'iso_byte_char'|
    'iso_incomplete'|
    'iso_misc'|
    'read'|
    'streams'|
    'write'.
classicprolog := 'classic_doc'|
    'dcg_doc'|
    'dcg_expansion'|
    'old_database'|
    'lists'|
    'sort'|
    'dict'|
    'strings'|
    'dec10_io'|
    'format'|
    'ttyout'|
    'dynmods'|
    'runtime_ops'.
annotatedprolog := 'assertions_doc'|
    'assertions_props'|
    'regtypes_doc'|
    'native_props'|
    'meta_props'|
    'isomodes_doc'|
    'basicmodes_doc'|
    'rtchecks_doc'.
% fdtypes
%	metatypes.pl \
miscprolog := 'system'|
    'prolog_sys'|
    'ctrlcclean'|
    'errhandle'|
    'fastrw'|
    'filenames'|
    'file_utils'|
    'file_locks'|
    'terms'|
    'tokenize'|
    'messages'|
    'pretty_print'|
    'assrt_write'|
    'librowser'|
    'expansion_tools'|
    'conc_aggregates'|
    'davinci'.
%     assrt_lib.pl \
%     byrd.pl \
% 	traces.pl \
%     conc_aggregates.pl \
%     events.pl \
%     fly.pl \
%     listing.pl \
%     loops.pl \
%     parse_spec.pl \
%     prompt.pl
extendprolog := 'pure_doc'|
    'argnames'|
    'andprolog'|
    'functions'|
    'hiord'|
    'global'|
    'odd'|
    'freeze'|
    'actmods'|
    'bf_doc'|
    'clpq_doc'|
    'clpr_doc'|
    'ociao_doc'|
    'class_doc'|
    'class_error_doc'|
    'objects_doc'|
    'objects_error_doc'|
    'objects_rt'.
% Driven by ociao_doc
%	ociao.pl \
%	objects.pl \
%	ociao.pl \
% Loop?
% 	class.pl \
%
%     cges.pl \
interfaces := 'foreign_interface'|
    'foreign_compilation'|
    'sockets'|
    'tcltk'|
    'pillow_doc'|
    'html'|
    'http'|
    'persdbrt'|
    'Examples'|
    'persdbrtsql'|
    'pl2sql'|
    'db_client'|
    'javart'|
    'jtopl'|
    'javasock'|
    'emacs'|
    'linda'.
adts := 'atom2term'|
    'counters'|
    'idlists'|
    'metaterms'|
    'numlists'|
    'patterns'|
    'graphs'|
    'ugraphs'|
    'wgraphs'|
    'lgraphs'|
    'queues'|
    'random'|
    'sets'|
    'vndict'.
%     arrays.pl \
%     bitcodesets.pl \
%     formulae.pl \
%     keys.pl \
%     llists.pl \
%     lsets.pl \
utilcomponents := 'fileinfo'|
    'viewpo'|
    'xrefs_doc'|
    'get_deps'|
    'pldiff'|
    'lpmake'|
    'ciao_get_arch_doc'.
%	collect_modules_doc.pl
