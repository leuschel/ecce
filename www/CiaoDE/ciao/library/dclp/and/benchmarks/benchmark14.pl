:- use_package(library('dclp/and')).
:- use_module(library(prolog_sys), [statistics/2]).

main(A,B,C,D,E) :-
	store([A, C], 1),
	store([B], 2),
	store([D], 3),
	store([E], 4),
	([A,C] in 0..10,
         A + 2 .=<. C) @ 1,
	([A,B] in 0..10,
         A + 3 .=<. B) @ 2,
	([B,D] in 0..10,
         D + 1 .=<. B) @ 3,
	([C,E] in 0..10,
         C + 3 .=<. E) @ 4,
	([B,E] in 0..10,
         B + 3 .=<. E) @ 5,
	([D,E] in 0..10,
         D + 2 .=<. E) @ 6,
	d_labeling([A,B,C,D,E]).
