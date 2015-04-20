
:- use_module(library(lists)).

a:- display('First'), nl.
a:- display('Second'), nl.
a:- display('Third and last'), nl.

b:- display('First'), nl.
b:- display('Second and last'), nl.


c:- display('First and last'), nl.

d:-
        append(A, B, [1,2,3]),
        display(A), nl,
        display(B), nl.
