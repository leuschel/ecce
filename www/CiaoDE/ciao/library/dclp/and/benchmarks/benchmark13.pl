:- use_package(library('dclp/and')).
:- use_module(library(prolog_sys), [statistics/2]).

main(X1,X2,X3,X4) :-
	store([X1], 3),
	store([X2], 2),
	store([X3], 1),
	store([X4], 4),
	(Q=[X3,X4],
	 Q in 1..4,
	 all_different([X3, X4]),
	 X3 .<>. X4 + 1,
	 X3 .<>. X4 - 1) @ 1,
	(Q2=[X2,X3,X4],
	 Q2 in 1..4,
	 all_different(Q2),
	 X2 .<>. X3 + 1,
	 X2 .<>. X3 - 1,
	 X2 .<>. X4 + 2,
	 X2 .<>. X4 - 2) @ 2,
	(Q3=[X1,X2,X3,X4],
	 Q3 in 1..4,
	 all_different(Q3),
	 X1 .<>. X2 + 1,
	 X1 .<>. X2 - 1,
	 X1 .<>. X3 + 2,
	 X1 .<>. X3 - 2,
	 X1 .<>. X4 + 3,
	 X1 .<>. X4 - 3) @ 3,
	(Q4=[X3,X4],
	 Q4 in 1..4,
	 X4 .>. X3) @ 4,
	d_labeling([X1,X2,X3,X4]).

% De aqui vemos que ****cada agente debe propagar todas las variables que ve***
% pero solo debe hacer labeling de las que le pertenecen.
