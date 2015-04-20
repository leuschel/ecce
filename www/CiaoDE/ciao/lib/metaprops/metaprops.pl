
:- use_package(hiord).

:- use_module(library('metaprops/meta_props'),[prop/2, regtype/2]).

:- multifile callme/2.

callme(P,X):- call(P,X), !.
