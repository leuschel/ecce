:- module(install_utils, [
        error_dir/1,
        img_dir/1
                         ], []).

:- use_package(assertions).

:- comment(author,"Isabel Mart@'{i}n Garc@'{i}a").


error_dir(Dir):-
        absolute_file_name(library(chartlib), '', '.pl', '.', _AbsFile, _AbsBase, AbsDir),
        atom_concat(AbsDir, '/errors/',Dir).
        

img_dir(Dir):-
        absolute_file_name(library(chartlib), '', '.pl', '.', _AbsFile, _AbsBase, AbsDir),
        atom_concat(AbsDir, '/images/',Dir).
