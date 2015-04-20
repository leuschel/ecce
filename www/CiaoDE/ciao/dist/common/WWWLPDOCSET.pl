:- module(_,_,[functions,assertions] ).

:- use_module(ciaosrc('dist/DISTSETTINGS')).
:- reexport(ciaosrc('dist/common/DISTLPDOCCOM')).

redefining(docformat/1).

docformat := ~wwwdocformat.
