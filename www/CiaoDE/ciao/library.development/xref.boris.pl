 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% xref.pl -- Referencias cruzadas usando arboles ordenados
 %% AFSID           : $__Header$
 %% Author          : Manuel Carro Li~nares
 %% Created On      : Thu Feb  3 14:05:23 1994
 %% Last Modified By: Manuel Carro
 %% Last Modified On: Tue Jul 30 17:08:03 1996
 %% Update Count    : 129
 %% Status          : Correct....?

 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% xref(+ListaFicheros+, +FicheroSalida+): Las referencias cruzadas
 %% correspondientes a ListaFicheros se imprimen en FicheroSalida
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xref(Fs, Salida):-
	seeing(OldIn),
 	telling(OldIn),
	ref_fich(Fs, Datos),
	escribe(Datos, Salida),
	see(OldIn),
	tell(OldIn).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% ref_fich(+ListaFicheros+, ?TablaDatos?): TablaDatos guarda las
 %% referencias correspondientes a ListaFicheros. Va abriendo
 %% ficheros, leyendolos y construyendo la estructura de datos.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ref_fich([], _).
ref_fich([F|Fs], Datos):-
	see(F),
	read(Clausula),
	anade_datos(Clausula, F, Datos),
	seen,
	ref_fich(Fs, Datos).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% escribe(?Datos+, +Salida+): la tabla Datos, conteniendo los datos
 %% de referencias cruzadas, se imprimen en el fichero Salida.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
escribe(Datos, Salida):-
 	tell(Salida),
 	escribe(Datos),
	nl,
 	told.


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% anade_datos(+Lectura+, +Fichero+, ?Datos?): se actualiza la tabla
 %% Datos tras haber leido Lectura (una clausula o fin de fichero) en
 %% Fichero
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

anade_datos(end_of_file, _, _):- !.         % Corte verde
anade_datos(Clausula, Fichero, Datos):-
	Clausula \== end_of_file,
	actualiza_estructura(Clausula, Fichero, Datos),
	read(OtraClausula),
	anade_datos(OtraClausula, Fichero, Datos).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% actualiza_estructura(+Clausula+, +Fichero+, ?Datos?): se anade a
 %% la tabla Datos los datos pertenecientes a la Clausula que aparece
 %% en Fichero
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

actualiza_estructura((Cabeza :- Cuerpo), Fichero, Datos):- !,  %% Verde
	def_pred(Datos, Cabeza, Fichero),
	actualiza_usados(Cuerpo, Fichero, Datos).
actualiza_estructura(Hecho, Fichero, Datos):-
	Hecho \== (_Cabeza :- _Cuerpo),
	def_pred(Datos, Hecho, Fichero).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% actualiza_usados(+Objetivo+, +Fichero+, ?Datos?): Objetivo es
 %% parte del cuerpo de una clausula que aparece en Fichero, y se
 %% anada a Datos.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

actualiza_usados((Meta, Metas), Fichero, Datos):- !,  %% Verde
	uso_pred(Datos, Meta, Fichero),
	actualiza_usados(Metas, Fichero, Datos).
actualiza_usados(Meta, Fichero, Datos):-
	Meta \== (_Meta, _Metas),
	uso_pred(Datos, Meta, Fichero).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Estructuras de los datos:
 %% Datos:          tabla(Ord_Fichs, Ord_Preds)
 %% Ord_Fichs:      fichero(Fichero*, Pred_Definidos, Pred_Usados)+
 %% Ord_Preds:      predicado(IdPred, Fichero_defi, Fichero_uso)+
 %% Pred_Definidos: predicado(IdPred, Fichero_defi, Fichero_uso)+
 %% Pred_Usados:    predicado(IdPred, Fichero_defi, Fichero_uso)*
 %% Fichero_defi:   Fichero
 %% Fichero_uso:    Fichero*
 %% Fichero:        nombre de fichero | ninguno
 %% IdPred:         identificador de predicado
 %%
 %% Todos los Elemento* estan programados como un arbol binario ordenado
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% def_pred(?Tabla?, +Cabeza+, +Fichero+): el predicado con cabeza
 %% Cabeza aparece definido en Fichero y se anade en Tabla
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

def_pred(tabla(Ord_Fichs, Ord_Preds), Cabeza, Fichero_def):-
	functor(Cabeza, Nombre, Aridad),
	PredInfo = predicado(Nombre/Aridad, Fichero_def, _F_uso),
	esta_en(Ord_Fichs, fichero(Fichero_def, Definidos, _Usados)),
	esta_en(Definidos, PredInfo),
	esta_en(Ord_Preds, PredInfo).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% uso_pred(?Tabla?, +Objetivo+, +Fichero+): Objetivo es parte de una
 %% clausula que aparece en Fichero, y se anade en Tabla.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

uso_pred(Tabla, Obj, Fichero_uso):-
	(
	    predicate_property(Obj, built_in) ->
	    true
	;
	    Tabla = tabla(Ord_Fichs, Ord_Preds),
	    functor(Obj, Nombre, Aridad),
	    PredInfo = predicado(Nombre/Aridad, _F_Def, Ficheros_uso),
	    esta_en(Ord_Preds, PredInfo),
	    esta_en(Ficheros_uso, Fichero_uso),
	    esta_en(Ord_Fichs, fichero(Fichero_uso, _Definidos, Usados)),
	    esta_en(Usados, PredInfo)
	).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% esta_en(?Arbol?, ?Elemento?): Elemento esta en Arbol
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

esta_en(arb(Elemento, _Izq, _Der), Elemento):- !.  %% Rojo
esta_en(arb(EsteElemento, _Izq, Der), Elemento):-
	EsteElemento @< Elemento,
	esta_en(Der, Elemento).
esta_en(arb(EsteElemento, Izq, _Der), Elemento):-
	EsteElemento @> Elemento,
	esta_en(Izq, Elemento).

 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% escribe(?Tabla+): Tabla se imprime en la salida estandar
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

escribe(tabla(Ord_Fichs, Ord_Preds)):-
 	escribe_fich(Ord_Fichs),
	format("~3|~w~30|~w~50|~w~n~n", ['Predicado', 'Definido en',
 	'Utilizado en']),
	escribe_pred(Ord_Preds).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% escribe_fich(?TablaFich+): imprime la informacion de referencias
 %% cruzadas ordenada por ficheros.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

escribe_fich(vacio):- !.			 %% Rojo
escribe_fich(arb(fichero(NombreFich, Definidos, Usados), Izq, Der)):-
	escribe_fich(Izq),
	format("Fichero ~w:~n~n", [NombreFich]),
	format("~3|~w~30|~w~n", ['Predicados definidos:', 'Utilizado en:']),
	escribe_definidos(Definidos),
	nl,
	format("~3|~w~30|~w~n", ['Predicados utilizados:', 'Definido en:']),
	escribe_utilizados(Usados), nl, nl,
	escribe_fich(Der).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% escribe_definidos(?TablaDefinidos+): escribe secuencialmente la
 %% tabla de predicados definidos en un fichero y en que otros
 %% ficheros se usan.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

escribe_definidos(vacio):- !.			 %% Rojo
escribe_definidos(arb(predicado(IdPred, _FDef, FUso), Izq, Der)):-
	escribe_definidos(Izq),
        format("~3|~w~30|", [IdPred]),
	escribe_arbol(FUso), nl,
	escribe_definidos(Der).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% escribe_utilizados(?TablaUtilizados+): escribe la tabla de
 %% predicados utilizados en un fichero y donde se definen.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

escribe_utilizados(vacio):- !.			 %% Rojo
escribe_utilizados(arb(predicado(IdPred, FDef, _FUso), Izq, Der)):-
	escribe_utilizados(Izq),
	completa(FDef),
        format("~3|~w~30|~w~n", [IdPred, FDef]),
	escribe_utilizados(Der).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% escribe_pred(?TablaPred+): escribe la tabla de predicados, en que
 %% fichero se definen y en que ficheros se usan.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

escribe_pred(vacio):- !.			 %%  Rojo
escribe_pred(arb(predicado(IdPred, FichDefi, FichUso), Izq, Der)):-
	escribe_pred(Izq),
	completa(FichDefi),
        format("~3|~w~30|~w~50|", [IdPred, FichDefi]),
	escribe_arbol(FichUso), nl,
	escribe_pred(Der).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% completa(?Nombre+): Si Nombre es una variable a la entrada, se
 %% unifica con el atomo '????'. En caso contrario no se hace nada.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

completa('????'):- !.                            %% Rojo
completa(_Nombre).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% escribe_arbol(Arb): escribe los elementos del arbol Arb en la
 %% salida estandar, separados por espacios, y rellena las variables
 %% libres con la constante 'vacio'. Los arboles vacios se imprimen
 %% con la constante '????'.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

escribe_arbol(vacio):- !, write('????').	 %% Rojo
escribe_arbol(Arb):- escribe_arbol_no_vacio(Arb).

escribe_arbol_no_vacio(vacio):- !.		 %% Una hoja; rojo
escribe_arbol_no_vacio(arb(Elemento, Izq, Der)):-
	escribe_arbol_no_vacio(Izq),
	format("~w ", [Elemento]),
	escribe_arbol_no_vacio(Der).
