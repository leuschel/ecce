:- module(tetrahedral_function, [test/1], [functions, lazy]).


:- function write_row/2.
write_row(X, X) := true
	        :- display(X), nl.
write_row(X, Y) := write_row(X, Y+1)
	        :- display(Y), display(' ').

:- function tetrahedral/1.
tetrahedral(0) := true.
tetrahedral(X) := tetrahedral(X-1)
	       :- _ = write_row(X, 1).

test(N) :-  _ = tetrahedral(N).
