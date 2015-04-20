
:- use_package([assertions,metaprops]).
:- comment(nodoc,assertions).
% does not work:
%:- comment(nodoc,metaprops).
:- comment(hide,callme/2).

:- comment(title,"Classical Prolog modes").
 
:- comment(author,"Manuel Hermenegildo").

:- comment(module,"This file defines a number of very simple
   ``@concept{modes}'' which are frequently useful in programs. These
   correspond to the modes used in classical Prolog texts with some
   simple addtions. Note that some of these modes use the same symbol
   as one of the ISO-modes (see @ref{ISO-Prolog modes}) but with subtly
   different meaning.").

:- comment(usage,":- use_package([assertions,basicmodes]).").

:- include(library(basicmodes)).

:- comment('+'/1,"Input value in argument.").
:- comment('-'/1,"No input value in argument.").
:- comment('?'/1,"Unspecified argument.").
:- comment('@'/1,"No output value in argument.").
:- comment(in/1,"Input argument.").
:- comment(out/1,"Output argument.").
:- comment(go/1,"Ground output (input/output argument).").
