:- module(dirs_manual,[]).
:- use_package([]).
:- multifile file_search_path/2.
:- dynamic file_search_path/2.

file_search_path(lpdoclib,'/home/clip/Systems/lpdoc-1.9/src').
