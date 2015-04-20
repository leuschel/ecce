:- module(ease, [ease/1, ease/2], []).

:- use_module(library(dec10_io)).
:- use_module(library(read)).
:- use_module(library(write)).
:- use_module(library(system)).
:- use_module(curve).

ease(In,Code):-
        see(In),
        read(X),
        seen,
        atom_concat(In, '.wrl', Out),
        curve_to_file(X, Code).
 %%         tell(Out),
 %%         write(Code),
 %%         told.
