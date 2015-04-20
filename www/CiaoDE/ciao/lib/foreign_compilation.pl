:- module(foreign_compilation, 
        [
            compiler_and_opts/2,
            linker_and_opts/2
        ], 
        [assertions, isomodes]).

:- use_module(library(system)).

:- comment(title,"Utilities for on-demand compilation of foreign files").

:- comment(author,"Manuel Carro").
:- comment(author,"Jose Morales"). 

:- comment(module, "This module provides two predicates which give the user
   information regarding how to compile external (C) files in order
   to link them with the Ciao Prolog engine at runtime.

   These predicates are not intended to be called directly by the end-user.  
   Instead,  a tool or module whose aim is generating dynamically 
   loadable files from source files should use the predicates in this file 
   in order to find out what are the proper compiler and linker to use, 
   and which options must be passed to them in the current architecture.").



:- discontiguous(foreign_compiler_options/4).
:- discontiguous(foreign_linker_options/4).

:- include(library(auto_compile_options)).

:- true pred compiler_and_opts(?Compiler, ?Opts) : 
        atom * list(atom)
 #"If you want to compile a foreign language file for dynamic linking in 
 the current operating system and architecture, you have to use
 the compiler @var{Compiler} and give it the options @var{Opts}. 
A variable in @var{Opts} means that no special option is needed.".
 
:- true pred linker_and_opts(?Linker, ?Options) : 
        atom * list(atom)
 #"If you want to link a foreign language file for dynamic linking in 
 the current operating system and architecture, you have to use
 the linker @var{Compiler} and gite it the options @var{Opts}.
 A variable in @var{Opts} means that no special option is needed.".


compiler_and_opts(Compiler, Options):-
        get_os(Os),
        get_arch(Arch),
        foreign_compiler_options(Os, Arch, Compiler, Options).

linker_and_opts(Linker, Options):-
        get_os(Os),
        get_arch(Arch),
        foreign_linker_options(Os, Arch, Linker, Options).
