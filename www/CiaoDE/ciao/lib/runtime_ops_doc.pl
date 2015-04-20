:- use_package([assertions]).
:- comment(nodoc,assertions).

:- comment(title,"Enabling operators at run-time").

:- comment(author, "Daniel Cabeza").

:- comment(module,"This library package allows the use of the statically
   defined operators of a module for the reading performed at run-time
   by the program that uses the module. Simply by using this package the 
   operator definitions appearing in the module are enabled during the 
   execution of the program.").

:- include(library(runtime_ops)).
