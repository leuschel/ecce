:- module(naturals_function, _, [functions, lazy]).

:- use_module(library(lists), [nth/3]).

:- lazy function generate_naturals_list/1.
generate_naturals_list(X) := [X | generate_naturals_list(X+1)].

test(Position) := ~nth(Position, generate_naturals_list(1)).
