:- use_package(library('dclp/and')).
:- use_module(library(prolog_sys), [statistics/2]).

main(X1,X2,X3,X4) :-
	store([X1], 1),
	store([X2], 2),
	store([X3, X4], 3),
	([X1,X2,X3,X4] in 1..4,
	 all_different([X1,X2,X3,X4]),
	 X1 .<>. X2 + 1,
	 X1 .<>. X2 - 1,
	 X1 .<>. X3 + 2,
	 X1 .<>. X3 - 2,
	 X1 .<>. X4 + 3,
	 X1 .<>. X4 - 3) @ 1,
	([X2,X3,X4] in 1..4,
	 all_different([X2,X3,X4]),
	 X2 .<>. X3 + 1,
	 X2 .<>. X3 - 1,
	 X2 .<>. X4 + 2,
	 X2 .<>. X4 - 2) @ 2,
	([X3,X4] in 1..4,
	 all_different([X3,X4]),
	 X3 .<>. X4 + 1,
	 X3 .<>. X4 - 1) @ 3,
	d_labeling([X1,X2,X3,X4]).

 % Nadie usa X1: Mal diseñado. Haciendo una reasignacion dinamica de la
 % prioridad de la fuente se arregla en  parte. Sin embargo, lo que no se
 % puede arreglar es la  distribucion erronea que el usuario ha hecho de
 % las variables al darle al de menor prioridad dos variables en propiedad
 % sin necesitar ninguna de los otros agentes. Esto hace que este agente
 % quede aislado de los demas durante labeling. Idea: Hacer un analisis de
 % la conectividad previo a la ejecucion.  
