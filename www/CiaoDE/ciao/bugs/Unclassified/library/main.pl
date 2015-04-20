
%% compile with: ciaoc -u devlib main

:- use_package([ ]).

:- use_module(library(mylib)).

main:- mylib:disp('hola').
