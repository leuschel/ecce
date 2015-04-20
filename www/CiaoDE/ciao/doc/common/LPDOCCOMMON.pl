:- module(_,_,[functions,assertions] ).
:- use_module(library(system),[current_env/2, working_directory/2, cd/1]).
:- use_module(library(terms),[atom_concat/2]).
:- use_module(library(distutils), [absolute_dir_name/2, extract_strings/3,
	pwd/1]).

:- use_module(ciaosrc('CIAOSETTINGS')).
:- use_module(ciaosrc('CIAOSHARED')).

:- reexport(ciaosrc('doc/common/CIAOLPDOCSHARED')).

:- reexport(ciaosrc('CIAOSETTINGS'),[lpdoclib/1]).

:- redefining(_).

ciaofilepath := 
	''|
 '/doc/common'|
 '/doc/readmes'.

filepath := ~atom_concat(~ciaosrc,~ciaofilepath).
% To be defined at lowel levels:
% mainfile := '***'.

ciaosystempath :=
	'/lib'|
 '/lib/compiler'|
 '/lib/debugger'|
 '/lib/engine'|
 '/lib/assertions'|
 '/lib/metatypes'|
 '/lib/regtypes'|
 '/lib/foreign_interface'|
 '/library'|
 '/library/actmods'|
 '/library/agent'|
 '/library/andorra'|
 '/library/andprolog'|
 '/library/argnames'|
 '/library/assertions'|
 '/library/bf'|
 '/library/builtintables'|
 '/library/byrdbox'|
 '/library/class'|
 '/library/class'|
 '/library/class/examples'|
 '/library/clpq'|
 '/library/clpr'|
 '/library/concurrency'|
 '/library/det_hook'|
 '/library/emacs'|
 '/library/factsdb'|
 '/library/file_locks'|
 '/library/fly'|
 '/library/freeze'|
 '/library/functions'|
 '/library/fuzzy'|
 '/library/graphs'|
 '/library/graphs'|
 '/library/id'|
 '/library/indexer'|
 '/library/interface'|
 '/library/javall'|
 '/library/javall/autofig'|
 '/library/librowser'|
 '/library/math'|
 '/library/make'|
 '/library/objects'|
 '/library/objects/examples'|
 '/library/persdb'|
 '/library/persdb/examples'|
 '/library/persdb_sql_common'|
 '/library/persdb_mysql'|
 '/library/persdb_odbc'|
 '/library/pillow'|
 '/library/random'|
 '/library/remote'|
 '/library/rtchecks'|
 '/library/sockets'|
 '/library/symfnames'|
 '/library/tcltk'|
 '/library/tcltk_obj'|
 '/library/toplevel'|
 '/library/tracing'|
 '/library/when'|
 '/library/xrefs'|
 '/library/xrefs/doc'|
 '/contrib'|
 '/contrib/time_analyzer'|
 '/contrib/tester'|
 '/contrib/modtester'|
 '/contrib/gnuplot'|
 '/contrib/ddlist'|
 '/contrib/chartlib'|
 '/contrib/lazy'|
 '/contrib/fd'|
 '/contrib/mycin'|
 '/contrib/provrml'|
 '/contrib/xdr_handle'|
 '/contrib/xml_path/doc'.

systempath := ~atom_concat(~ciaosrc, ~ciaosystempath).


% To be defined at lower levels
% component := '***'.

index := concept | lib | pred | prop | regtype | decl | global.
% index := prop.
% index := modedef.

infodir_headfile := ~atom_concat([~ciaosrc,'/doc/common/CiaoHead.info']).
infodir_tailfile := ~atom_concat([~ciaosrc,'/doc/common/CiaoTail.info']).
