:- module(_,_,[functions,assertions] ).
:- use_module(library(terms),[atom_concat/2]).
:- use_module(library(distutils)).
:- use_module(ciaosrc('CIAOSETTINGS')).
:- use_module(ciaosrc('CIAOSHARED')).

:- reexport(ciaosrc('doc/common/LPDOCCOMMON')).

:- reexport(ciaosrc('CIAOSETTINGS'), [lpdoclib/1, docdir/1]).
:- reexport(ciaodesrc('CIAODESETTINGS'), [docformat/1]).

:- redefining(_).

mainfile := 'ciao'.

ciaofilepatha :=
 '/doc/reference'|
 '/shell'|
 '/ciaoc'|
 '/engine'|
 '/emacs-mode'|
 '/etc'|
 '/library/pillow/dist/doc'.

ciaofilepathref := ~ciaofilepath | ~ciaofilepatha.

filepath := ~atom_concat(~ciaosrc,~ciaofilepathref).

component :=
 ~getstarted|
 'DevEnv'|
 ~devenv|
 'Builtins'|
 ~refcomponents|
 'IsoProlog'|
 ~isoprolog|
 'ClassicProlog'|
 ~classicprolog|
 'AnnotatedProlog'|
 ~annotatedprolog|
 'MiscProlog'|
 ~miscprolog|
 'ExtendProlog'|
 ~extendprolog|
 'Interfaces'|
 ~interfaces|
 'ADTs'|
 ~adts|
 'ciao-utilities'|
 ~utilcomponents|
 'Contrib'|
 ~contrib|
 'Append'|
 ~installation.

getstarted :=
 'GetStartUnix'|
 'GetStartWin32'.

installation :=
 'Install'|
 'InstallWin32bin'|
 'BeyondInstall'.

devenv :=
 'ciaoc'|
 'toplevel_doc'|
 'debugger_doc'|
   'debugger'|
 'ciao-shell'|
 'libpaths'|
 'CiaoMode'.

%	toplev'|

refcomponents :=
 'modules'|
 'loading_code'|
 'basiccontrol'|
 'builtin_directives'|
 'basic_props'|
 'term_typing'|
 'term_basic'|
 'term_compare'|
 'atomic_basic'|
 'arithmetic'|
 'streams_basic'|
 'io_basic'|
 'exceptions'|
 'prolog_flags'|
 'data_facts'|
 'syntax_extensions'|
 'io_aux'|
 'attributes'|
 'system_info'|
 'default_predicates'.

%              prolog.src \
%              examples.src 

% Should not be used, so we do not document them
% 	internals'|

% Other
% 	mexpand'|

% Taken out
%	ciaoengine'.

isoprolog :=
 'iso_doc'|
 'aggregates'|
 'dynamic'|
 'read'|
 'write'|
 'operators'|
 'iso_byte_char'|
 'iso_misc'|
 'iso_incomplete'.

classicprolog :=
 'dcg_doc'|
   'dcg_expansion'|
 'format'|
 'lists'|
 'sort'|
 'compiler'|
 'between'|
 'system'|
 'prolog_sys'|
 'dec10_io'|
 'old_database'|
 'ttyout'|
 'runtime_ops_doc'.

%    classic_doc'|

annotatedprolog :=
 'assertions_doc'|
   'assertions_props'|
 'regtypes_doc'|
   'native_props'|
 'isomodes_doc'|
 'basicmodes_doc'|
 'rtchecks_doc'.

% fdtypes
%	metatypes'|
%	meta_props'|

miscprolog :=
 'streams'|
 'dict'|
 'strings'|
 'messages'|
 'io_alias_redirection'|
 'atom2term'|
 'ctrlcclean'|
 'errhandle'|
 'fastrw'|
 'filenames'|
 'symfnames'|
 'file_utils'|
 'file_locks'|
 'formulae'|
 'terms'|
 'terms_check'|
 'terms_vars'|
 'cyclic_terms'|
 'pretty_print'|
 'assrt_write'|
 'librowser'|
 'expansion_tools'|
 'concurrency'|
 'conc_aggregates'|
 'sockets'|
 'sockets_io'|
 'make_doc'|
 'make_rt'|
 'system_extra'.

%    tokenize'|
%     assrt_lib'|
%     byrd'|
% 	traces'|
%     events'|
%     fly'|
%     listing'|
%     loops'|
%     parse_spec'|
%     prompt'.

extendprolog :=
 'pure_doc'|
 'indexer_doc'|
 'hiord_rt'|
 'hiordlib'|
 'argnames_doc'|
 'functions_doc'|
 'global'|
% 'andprolog_doc'|
 'andorra_doc'|
 'det_hook_rt'|
 'odd'|
 'freeze'|
 'when'|
 'actmods_doc'|
 'bf_doc'|
 'id_doc'|
 'clpq_doc'|
 'clpr_doc'|
 'fuzzy_doc'|
 'ociao_doc'|
   'class_doc'|
   'objects_doc'|
   'objects_rt'|
 'remote_doc'. 

% Driven by ociao_doc
%	ociao'|
%	objects'|
%	ociao'|

% Loop?
% 	class'|
%
%     cges'|


interfaces :=
 'foreign_interface_doc'|
 'foreign_interface_properties'|
 'foreign_compilation'|
 'build_foreign_interface'|
 'davinci'|
 'tcltk'|
   'tcltk_low_level'|
 'window_class_doc'|
   'widget_class_doc'|
     'menu_class_doc'|
     'canvas_class_doc'|
     'button_class_doc'|
     'checkbutton_class_doc'|
     'radiobutton_class_doc'|
     'entry_class_doc'|
     'label_class_doc'|
     'menubutton_class_doc'|
     'menu_entry_class_doc'|
   'shape_class_doc'|
     'arc_class_doc'|
     'oval_class_doc'|
     'poly_class_doc'|
     'line_class_doc'|
     'text_class_doc'|
 'pillow_doc'|
   'html'|
   'http'|
   'pillow_types'|
 'persdbrt'|
   'Examples'|
 'factsdb_rt'|
 'persdbrt_mysql'|
   'pl2sql'|
   'mysql_client'|
   'db_client_types'|
   'sqltypes'|
   'persdbtr_sql'|
   'pl2sqlinsert'|
  'javart'|
   'jtopl'|
 'javasock'|
 'emacs'|
 'linda'.

%    persdb_sql_common'|
%	db_client'|


adts :=
 'arrays'|
 'counters'|
 'idlists'|
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

%     bitcodesets'|
%     formulae'|
%     keys'|
%     llists'|
%     lsets'|

utilcomponents :=
 'cleandirs'|
 'fileinfo'|
 'viewpo'|
 'xrefs_doc'|
 'callgraph'|
 'get_deps'|
 'pldiff'|
 'lpmake'|
 'ciao_get_arch_doc'|
 'compiler_output'|
 'auto_compile_ciao_doc'|
 'collect_modules_doc'. 

%	collect_modules_doc'.

contrib :=
 'fd_doc'|
 'mycin_doc'|
 'xdr_handle'|
 'xml_path_doc'|
 'ddlist'|
 'time_analyzer'|
 'gnuplot'|
 'modtester'|
 'tester'|
 'lazy'|
 'chartlib'|
   'bltclass'|
   'chartlib_errhandle'|
   'color_pattern'|
   'genbar1'|
   'genbar2'|
   'genbar3'|
   'genbar4'|
   'gengraph1'|
   'gengraph2'|
   'genmultibar'|
   'table_widget1'|
   'table_widget2'|
   'table_widget3'|
   'table_widget4'|
   'test_format'|
 'provrml'|
   'boundary'|
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
   'tokeniser'.

fileoption(~mainfile)  := '-nopatches'.
fileoption(~component) := '-noisoline'|'-noengmods'|'-propmods'|'-nochangelog'.

htmlstyle := ~atom_concat([~lpdoclib,'/default.css']).

htmlindex_headfile := ~atom_concat([~ciaosrc,'/doc/common/CiaoHead.html']).

htmlindex_tailfile := ~atom_concat([~ciaosrc,'/doc/common/CiaoTail.html']).
