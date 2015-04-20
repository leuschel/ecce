:- module(_,_,[functions,assertions] ).
:- use_module(library(system),[current_env/2, working_directory/2, cd/1]).
:- use_module(library(terms),[atom_concat/2]).
:- use_module(library(distutils), [absolute_dir_name/2, extract_strings/3]).
:- use_module(ciaosrc('CIAOSETTINGS')).
:- use_module(ciaosrc('CIAOSHARED')).

:- reexport(ciaosrc('doc/common/LPDOCCOMMON')).
:- reexport(ciaosrc('CIAOSETTINGS'), [docdir/1]).
:- redefining(component/1).

component := 'INSTALLATION'.
component := 'README'.

:- redefining(mainfile/1).

%mainfile := ~current_env('MAINFILE').
mainfile(_) :- fail.

:- redefining(index/1).

index(_) :- fail.

:- redefining(filepath/1).

filepath := '.' | '../doc'.

htmlstyle := ~atom_concat([~lpdoclib,'/default.css']).

htmlindex_headfile := ~atom_concat([~ciaosrc,'/doc/common/CiaoHead.html']).

htmlindex_tailfile := ~atom_concat([~ciaosrc,'/doc/common/CiaoTail.html']).
