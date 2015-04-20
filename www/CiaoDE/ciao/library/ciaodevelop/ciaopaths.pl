
:- module(ciaopaths,[],[]).

:- multifile library_directory/1.
:- dynamic library_directory/1.

library_directory('/home/clip/Systems/ciao/lib').
library_directory('/home/clip/Systems/ciao/library').

:- multifile file_search_path/2.
:- dynamic file_search_path/2.

file_search_path(etc,'/home/clip/Systems/ciao/etc').
