% Forces Failure
:- use_package(library('dclp/and')).
:- use_module(library(prolog_sys), [statistics/2]).

dec_seqs(N, L) :-
	do_seqs(N,_, [], L),
	display(L), nl,
	d_labeling(L).

do_seqs(0,0, Acc, Acc).
do_seqs(N, A, Acc, [B|L]) :-
	N > 0,
	store([A], N),
	N1 is N - 1,
	do_seqs(N1, B, [A|Acc], L),
	(A in 0..99,
	 A .<. 2*N + 2,
	 A .=. B + N) @ N.
