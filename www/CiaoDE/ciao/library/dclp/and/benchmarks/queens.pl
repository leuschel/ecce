:- use_package(library('dclp/and')).
:- use_module(library(prolog_sys), [statistics/2]).

queens(N, Qs) :-
	constrain_values(N, N, Qs), !,
	d_labeling(Qs).

constrain_values(0, _N, []).
constrain_values(N, Range, [X|Xs]):-
	N > 0, 
	store([X], N), 
	N1 is N - 1,
	constrain_values(N1, Range, Xs),
	no_attack(Xs, X, 1, N),
	(X in 1 .. Range, 
	 all_different([X|Xs])) @ N.

no_attack([], _Queen, _Nb, _).
no_attack([Y|Ys], Queen, Nb, N):-
	Nb1 is Nb + 1,
	no_attack(Ys, Queen, Nb1, N),
	(Queen .<>. Y + Nb,
	 Queen .<>. Y - Nb) @ N.
