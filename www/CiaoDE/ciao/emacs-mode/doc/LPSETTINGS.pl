:- module(_,_,[functions,assertions] ).
% :- use_module(library(terms),[atom_concat/2]).
:- use_module(library(distutils)).
:- use_module(ciaosrc('CIAOSETTINGS')).
:- use_module(ciaosrc('CIAOSHARED')).

:- reexport(ciaosrc('doc/common/LPDOCCOMMON')).

:- reexport(ciaosrc('CIAOSETTINGS'), [lpdoclib/1, docdir/1, docformat/1]).

:- redefining(_).

mainfile := 'CiaoMode'.

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

component := 'write'.

fileoption(~mainfile)  := '-nopatches'.
fileoption(~component) := '-noisoline'|'-noengmods'|'-propmods'|'-nochangelog'.

htmlstyle := ~atom_concat([~lpdoclib,'/default.css']).

htmlindex_headfile := ~atom_concat([~ciaosrc,'/doc/common/CiaoHead.html']).

htmlindex_tailfile := ~atom_concat([~ciaosrc,'/doc/common/CiaoTail.html']).
