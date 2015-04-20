
:- module(pure,[p/0]). %,pure).

:- data p/1.

p:- true, call(_), consult_fact(p(_)).
