:- use_package(library('dclp/and')).
:- use_module(library(prolog_sys), [statistics/2]).

main(X,Y):-
%	p([a(X,1),a(Y,2)]),
	q(X,Y),
	r(X,Y),
	d_labeling([X,Y]).
p([]).
p([X|Xs]) :-
	X = a(V,Id),
	store([V], Id),
	p(Xs).

q(X,Y) :-
	(X in 1..2, 
	 X .>. Y) @ 1.

r(X,Y) :-
	(Y in 1..2, 
	 X .<>. Y) @ 2.
