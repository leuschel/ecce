:- use_package(library('dclp/and')).
:- use_module(library(prolog_sys), [statistics/2]).

% solve the magic square puzzle for a 3*3 board

main(Vars) :-
	Vars = [X1,X2,X3,X4,X5,X6,X7,X8,X9],
	Q = [X1,X5,X9,X3,X7],
	store(Q, 1),
	store([X2, X4, X6, X8], 2),
	(Q in 1..9,
	 all_different(Vars),
	 X1+ X5 + X9 .=. 15,
	 X3+ X5 + X7 .=. 15) @ 1,

	(Vars in 1..9,
	 all_different(Vars),
	 X1+ X2 + X3 .=. 15,
	 X4+ X5 + X6 .=. 15,
	 X7+ X8 + X9 .=. 15) @ 2,

	(Vars in 1..9,
	 all_different(Vars),
	 X1+ X4 + X7 .=. 15,
	 X2+ X5 + X8 .=. 15,
	 X3+ X6 + X9 .=. 15) @ 3,

	d_labeling(Vars).
