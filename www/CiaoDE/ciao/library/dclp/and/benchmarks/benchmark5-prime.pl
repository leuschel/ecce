:- use_package(library('dclp/and')).
:- use_module(library(prolog_sys), [statistics/2]).

main(X1,X2,X3,X4) :-
	([X3,X4] in 1..4,
	 all_different([X3,X4]),
	 X3 .<>. X4 + 1,
	 X3 .<>. X4 - 1) @ 1,
	([X2,X3,X4] in 1..4,
	 all_different([X2,X3,X4]),
	 X2 .<>. X3 + 1,
	 X2 .<>. X3 - 1,
	 X2 .<>. X4 + 2,
	 X2 .<>. X4 - 2) @ 2,
	([X1,X2,X3,X4] in 1..4,
	 all_different([X1,X2,X3,X4]),
	 X1 .<>. X2 + 1,
	 X1 .<>. X2 - 1,
	 X1 .<>. X3 + 2,
	 X1 .<>. X3 - 2,
	 X1 .<>. X4 + 3,
	 X1 .<>. X4 - 3) @ 3,
	d_labeling([X1,X2,X3,X4]).
