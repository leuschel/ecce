:- module(_,_,[functions]).

:- use_module(library(terms)).
:- use_module(library(system)).
:- use_module(library(distutils)).

:- use_module(ciaosrc('dist/DISTSETTINGS')).

%:- reexport(ciaosrc('doc/common/LPDOCCOMMON')).
:- reexport(ciaosrc('doc/reference/LPSETTINGS')).

%:- reexport('../lib/DEFAULTS',[docformat/1]).

:- redefining(docdir/1).
:- redefining(htmlstyle/1).
:- redefining(htmlindex_headfile/1).
:- redefining(htmlindex_tailfile/1).

docdir := ~distdir.

htmlstyle := ~atom_concat([~ciaosrc,'/dist/ciao.css']).

htmlindex_headfile := ~atom_concat([~ciaosrc,'/dist/CiaoDistHead.html']).
htmlindex_tailfile := ~atom_concat([~ciaosrc,'/dist/CiaoDistTail.html']).
