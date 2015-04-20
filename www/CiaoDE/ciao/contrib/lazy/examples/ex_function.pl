:- module(ex_function,
	[
	    nats/1,
	    nat/2,
	    odds/1,
	    even/1,
	    ones/1,
	    squares/1,
	    odd_squares/1,
	    even_squares/1,
	    cubes/1,
	    test_takeWhile/1,
	    test_dropWhile/1
	], 
	[functions, hiord, lazy]).

:- use_module(library('lazy/lazy_lib'), _).


:- lazy function nats/0.
nats := ~nums_from(0).

:- function nat/1.
nat(N) := ~take(N, nats).	

:- lazy function odds/0.
odds := ~nums_from_inc(1, 2).

:- lazy function even/0.
even := ~nums_from_inc(0, 2).

:- lazy function ones/0.
ones := ~repeat(1).

:- lazy function squares/0.
squares := ~lazy_map(nats, (_(X, Y) :- Y is X * X)).

:- lazy function odd_squares/0.
odd_squares := ~lazy_map(odds, (_(X, Y) :- Y is X * X)).

:- lazy function even_squares/0.
even_squares := ~lazy_map(even, (_(X, Y) :- Y is X * X)).

:- lazy function cubes/0.
cubes := ~lazy_map(nats, (_(X, Y) :- Y is X * X * X)).

:- function test_takeWhile/0.
test_takeWhile := ~takeWhile((_(X) :- X =< 5), ~nums_from(0)).

:- function test_dropWhile/0.
test_dropWhile := ~take(3, ~dropWhile((_(X):-X=<7), ~nums_from(0))).
