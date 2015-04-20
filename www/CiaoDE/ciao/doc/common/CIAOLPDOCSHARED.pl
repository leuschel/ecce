:- module(_,_,[functions,assertions] ).
:- use_module(library(system),[current_env/2, working_directory/2, cd/1]).
:- use_module(library(terms),[atom_concat/2]).
:- use_module(ciaosrc('CIAOSETTINGS')).
:- use_module(ciaosrc('CIAOSHARED')).

% fileoption(_)     := '-v'.
% fileoption(_)     := '-nobugs'.
fileoption(_) :=
 '-modes'|
 '-nopatches'|
 '-noisoline'|
 '-noengmods'|
 '-propmods'|
 '-nochangelog'.

libtexinfo := 'yes'.

pathsfile := ~atom_concat(~ciaosrc, '/doc/common/doc_ops.pl').

:- reexport(ciaosrc('CIAOSHARED'), [bibfile/1]).

startpage := 1.

papertype := afourpaper.

compresscommand := 'gzip -f'.

compressext := 'gz'.

datapermissions(perm(rw,rw,r)).

execpermissions(perm(rwx,rwx,rx)).

infodir_headfile := ~atom_concat([~ciaosrc,'/doc/common/CiaoHead.info']).
infodir_tailfile := ~atom_concat([~ciaosrc,'/doc/common/CiaoTail.info']).
