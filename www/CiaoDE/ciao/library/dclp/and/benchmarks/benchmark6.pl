:- use_package(library('dclp/and')).
:- use_module(library(prolog_sys), [statistics/2]).

main(X1,X2,X3,X4) :-
	store([X1, X2], 1),
	store([X3], 2),
	store([X4], 3),
	store([], 4),
	store([], 5), 
	store([], 6),
	(Q=[X1,X2],
	 Q in 1..4,
	 all_different(Q),
	 X1 .<>. X2 + 1,
	 X1 .<>. X2 - 1 ) @ 1,
	(Q1=[X1,X3],
	 Q1 in 1..4,
	 all_different(Q1),
	 X1 .<>. X3 + 2,
	 X1 .<>. X3 - 2) @ 2,
	(Q2=[X1,X4],
	 Q2 in 1..4,
	 all_different(Q2),
	 X1 .<>. X4 + 3,
	 X1 .<>. X4 - 3) @ 3,
	(Q3=[X2,X3],
	 Q3 in 1..4,
	 all_different(Q3),
	 X2 .<>. X3 + 1,
	 X2 .<>. X3 - 1) @ 4,
	(Q4=[X2,X4],
	 Q4 in 1..4,
	 all_different(Q4),
	 X2 .<>. X4 + 2,
	 X2 .<>. X4 - 2) @ 5,
	(Q5=[X3,X4],
	 Q5 in 1..4,
	 all_different(Q5),
	 X3 .<>. X4 + 1,
	 X3 .<>. X4 - 1) @ 6,
	d_labeling([X1,X2,X3,X4]).
