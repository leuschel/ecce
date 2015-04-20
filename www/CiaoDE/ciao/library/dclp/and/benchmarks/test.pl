:- use_package(fd).

main(A,B,C,D,E) :-
	[A,B,C,D,E] in 0..10,
       A + 2 .=<. C,
       A + 3 .=<. B,
       D + 1 .=<. B,
       C + 3 .=<. E,
       B + 3 .=<. E,
       D + 2 .=<. E,
       labeling([A,B,C,D,E]).
