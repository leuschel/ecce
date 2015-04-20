:- use_package(library('dclp/and')).
:- use_module(library(prolog_sys), [statistics/2]).

dec_seqs(N, L) :-
	do_seqs(N,N,_,L),
	d_labeling(L).

do_seqs(0,_,0,[]).
do_seqs(N, Max, A, [A|L]) :-
	N > 0,
	store([A], N),
	N1 is N - 1,
	do_seqs(N1, Max, B, L),
	(A in 0..9,
	 A .=. B + N) @ N.
