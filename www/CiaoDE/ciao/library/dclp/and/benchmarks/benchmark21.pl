:- use_package(library('dclp/and')).
:- use_module(library(prolog_sys), [statistics/2]).

main(Encaja) :-
	Encaja = [Fisico, Psicologico, Pruebas],
 	store([Fisico], 1),
 	store([Psicologico], 2),
 	store([Pruebas], 3),

	(Psicologico in 3..10,
	 Fisico * Psicologico .>=. 50) @ 1,

	(Fisico in 4..10,
	 Psicologico .>. Fisico) @ 2,

	(Pruebas in 7..10,
	 Pruebas .>=. Fisico,
	 Pruebas .>=. Psicologico) @ 3,

	d_labeling(Encaja).
