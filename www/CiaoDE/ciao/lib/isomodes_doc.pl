
:- use_package([assertions]).
:- comment(nodoc,assertions).
% does not work:
%:- comment(nodoc,metaprops).
:- comment(hide,callme/2).

:- comment(title,"ISO-Prolog modes").
 
:- comment(author,"Daniel Cabeza").
:- comment(author,"Manuel Hermenegildo").
 
:- comment(module,"This file defines the ``@concept{modes}''
   used in the documentation of the ISO-Prolog standard.
   See also @ref{Classical Prolog modes} for an alternative set of
   modes.").

:- comment(usage,"@tt{:- use_package([assertions,isomodes]).}").

:- include(library(isomodes)).

:- comment('?'/1,"Unspecified argument.").
:- comment('*'/1,"Unspecified argument.").
