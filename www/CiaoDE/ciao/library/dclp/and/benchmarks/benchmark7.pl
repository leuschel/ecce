:- use_package(library('dclp/and')).
:- use_module(library(prolog_sys), [statistics/2]).

main(X1,X2,X3,X4) :-
	store([X1], 3),
	store([X2], 2),
	store([X3, X4], 1),
	(Q=[X3,X4],
	 Q in 1..4,
	 all_different(Q),
	 X3 .<>. X4 + 1,
	 X3 .<>. X4 - 1) @ 1,
	(Q1=[X2,X3,X4],
	 Q1 in 1..4,
	 all_different(Q1),
	 X2 .<>. X3 + 1,
	 X2 .<>. X3 - 1,
	 X2 .<>. X4 + 2,
	 X2 .<>. X4 - 2) @ 2,
	(Q2=[X1,X2,X3,X4],
	 Q2 in 1..4,
	 all_different(Q2),
	 X1 .<>. X2 + 1,
	 X1 .<>. X2 - 1,
	 X1 .<>. X3 + 2,
	 X1 .<>. X3 - 2,
	 X1 .<>. X4 + 3,
	 X1 .<>. X4 - 3) @ 3,
	 d_labeling([X1,X2,X3,X4]).
