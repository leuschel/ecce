
%% Has bug
:- module(m1,[m1doit/1,foo/1]).
%% No bug
%:- module(m1,[m1doit/1]).

:- use_module(m2,[foo/1]).

m1doit(X) :-
	asserta_fact(foo(a)),
	foo(X).
