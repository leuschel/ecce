
:- module(meta,[mycall/1]).
:- use_module(builtin).
:- meta_predicate(mycall(:)).
:- meta_predicate(mycall1(:)).

mycall(X) :- X.

mycall1(_X) :- Y=p, Y.

mycall2(X) :- X.
