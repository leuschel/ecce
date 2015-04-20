:- use_module(library(format),[format/2]).
:- use_module(library(lists),[append/3,select/3]).  %% MCL added select/3
:-ensure_loaded(con_concepto).
:-ensure_loaded(aso_asociacion).
:-ensure_loaded(val_valor).
:-ensure_loaded(ele_elemento).

:-ensure_loaded(entry).

%% MCL added definition
delete(In, Element, Out):- select(Element, In, Out).

main:- main(1, _).  %% MCL added

main(Numero,Camino):-
 	entry(Numero,Frase,L),
	convert_list(0,L,LC),
        format("Frase: ~w~n~n",[Frase]),
	format("~n~n==========Lista de Entrada========~n",[]),
	write_list(LC),
	unify_list(LC,LU,[],Camino),
	format("~n~n==========Lista Unificada=========~n",[]),
	write_list(LU),
        coste(Camino).

main(Numero,Fichero,Camino):-
        entry(Numero,Frase,L),
        convert_list(0,L,LC),
        open(Fichero,write,S),
        set_output(S),
        format("Frase: ~w~n~n",[Frase]),
        format("~n~n==========Lista de Entrada========~n",[]),
        write_list(LC),
        unify_list(LC,LU,[],Camino),
        format("~n~n==========Lista Unificada=========~n",[]),
        write_list(LU),
        coste(Camino),
        close(S).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%        convert_list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


 %% convert_list([],_,[]).
 %% convert_list([Token|Tail_Token],Index,Tail):-
 %% 	parse(Index,Token,Token_Parsed),
 %% 	convert(Token_Parsed, Elem),
 %%         !,  %% Imagino que ya no hace falta, depende de 
 %%         (
 %%             Elem = null ->
 %%             convert_list(Tail_Token, Index, Tail)
 %%           ;
 %%             Tail = [Elem|RestParsed],
 %%             Index1 is Index + 1,
 %%             convert_list(Tail_Token, Index1, RestParsed)
 %%         ).

convert_list(_,[],[]).
convert_list(Index,[Token|Tail_Token],Tail_Elem):-
	parse(Index,Token,Token_Parsed),
	convert(Token_Parsed,null),
        !,
	convert_list(Index,Tail_Token,Tail_Elem).
convert_list(Index,[Token|Tail_Token],[Elem|Tail_Elem]):-
	parse(Index,Token,Token_Parsed),
	convert(Token_Parsed,Elem),
        Index1 is Index + 1,
	convert_list(Index1,Tail_Token,Tail_Elem).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%        parse     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse(Index,([A1,A2,A3,A4,C1,C2,Id1,Id2,Id3,Id4],Val_ini,Val_fin,Fun),
		token(Index,Area,Concepto,Identificador,Val_ini,Val_fin,Fun)):-
	atom_codes(Area,[A1,A2,A3,A4]),
	atom_codes(Concepto,[C1,C2]),
	atom_codes(Identificador,[Id1,Id2,Id3,Id4]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%        unify_left list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


unify_list([],[],C,C).

unify_list([X],[X],C,C).

unify_list([H1,H2|Tail],List_Unified,Camino,Camino_Nuevo):-
	unificar(H1,H2,H_Unified,Camino,Camino_1),
	unify_list([H_Unified|Tail],List_Unified,Camino_1,Camino_Nuevo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%         unificar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unificar(Elemento_1, Elemento_2, Elemento_U, Camino, Camino):-
      unificar_directamente(Elemento_1, Elemento_2, Elemento_U).

unificar(Elemento_1, Elemento_2, Elemento_U, Camino, Camino):-
      unificar_izda(Elemento_1, Elemento_2, Elemento_U).

unificar(Elemento_1, Elemento_2, Elemento_U, Camino, Camino_Nuevo):-
    extender(Elemento_2, Elemento_2_Extendido, Camino, Camino_1),
    unificar(Elemento_1, Elemento_2_Extendido, Elemento_U, Camino_1, 
             Camino_Nuevo).

unificar_directamente( elemento(Token, Semantica, Pragmatica, Lista_Asoc_1),
                       elemento(Token, Semantica, Pragmatica, Lista_Asoc_2),
                       elemento(Token, Semantica, Pragmatica, Lista_Asoc)):-
%%  format("[Debug] Unificando directamente~n"),
%%  write_sem_set(elemento(Token,Semantica,Pragmatica,Lista_Asoc_1),1),
%%  write_sem_set(elemento(Token,Semantica,Pragmatica,Lista_Asoc_2),1),
%%  format("**************************************************************~n"),
%%  format("********************************************************HECHO.~n"),
  append(Lista_Asoc_1,Lista_Asoc_2,Lista_Asoc).


unificar_izda( elemento(Token_1,Semantica_1,Pragmatica_1,Lista_Asoc_1),
            elemento(Token_2,Semantica_2,Pragmatica_2,Lista_Asoc_2),
            elemento(Token_1,Semantica_1,Pragmatica_1,Lista_Asoc_Unificada)):-
%%    format("[Debug] Unificando por hijo izda~n"),
%%    write_sem_set(elemento(Token_1,Semantica_1,Pragmatica_1,Lista_Asoc_1),1),
%%    write_sem_set(elemento(Token_2,Semantica_2,Pragmatica_2,Lista_Asoc_2),1),
%%    format("************************************************************~n"),
    member(asoc(Token_Asoc,Semantica_Asoc,Elemento_Asoc),Lista_Asoc_1),
    delete(Lista_Asoc_1,asoc(Token_Asoc,Semantica_Asoc,Elemento_Asoc),
           Resto_Lista_Asoc_1),
    unificar_izda(Elemento_Asoc,elemento(Token_2,Semantica_2,Pragmatica_2,
                  Lista_Asoc_2),Elemento_Unificado),
    Lista_Asoc_Unificada = 
      [asoc(Token_Asoc,Semantica_Asoc,Elemento_Unificado)|Resto_Lista_Asoc_1].
%%  format("********************************************************HECHO.~n").

unificar_izda( elemento(Token_1,Semantica_1,Pragmatica_1,Lista_Asoc_1),
            elemento(Token_2,Semantica_2,Pragmatica_2,Lista_Asoc_2),
            elemento(Token_1,Semantica_1,Pragmatica_1,Lista_Asoc_Unificada)):-

%%    format("[Debug] Unificando por hijo izda~n"),
%%    write_sem_set(elemento(Token_1,Semantica_1,Pragmatica_1,Lista_Asoc_1),1),
%%    write_sem_set(elemento(Token_2,Semantica_2,Pragmatica_2,Lista_Asoc_2),1),
%%    format("************************************************************~n"),
    member(asoc(Token_Asoc,Semantica_Asoc,Elemento_Asoc),Lista_Asoc_1),
    delete(Lista_Asoc_1,asoc(Token_Asoc,Semantica_Asoc,Elemento_Asoc),
           Resto_Lista_Asoc_1),
    unificar_directamente(Elemento_Asoc,elemento(Token_2,Semantica_2,
           Pragmatica_2,Lista_Asoc_2),Elemento_Unificado),
    Lista_Asoc_Unificada = 
       [asoc(Token_Asoc,Semantica_Asoc,Elemento_Unificado)|Resto_Lista_Asoc_1].
%     format("*****************************************************HECHO.~n").



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%           extender
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Extendiendo un Elemento
extender(elemento(token(Area_1,Cat_1,Id_1,Vi_1,Vf_1,Fun_1),
         Semantica_1,Pragmatica_1,Lista_1),
	     elemento(
		      token(Area_S,Cat_S,Id_S,_Vi_S,_Vf_S,_Fun_S),
		      semantica(_Cuan_S,_Pol_S,Desc_S,Nat_S,_Tipo_S),
		      _,
		      [
		       asoc(
			    token(Area_A,'AS',Id_A,_Vi_A,_Vf_A,_Fun_A),
			    semantica(UML_A,Tipo_A,Desc_A,Nat_A,Card_A),
			    elemento(
				     token(Area_1,Cat_1,Id_1,Vi_1,Vf_1,Fun_1),
				     Semantica_1,
				     Pragmatica_1,
				     Lista_1))
		       ]),
           Camino,
           [asociacion(token(Area_A,'AS',Id_A),token(Area_S,Cat_S,Id_S),
                       token(Area_1,Cat_1,Id_1))|Camino]):-
%%    format("[Debug] Extendiendo~n"),
    concepto(Area_A,'AS',Id_A,_,Desc_A,_,Nat_A,_,_),	 
    asociacion(Area_A,'AS',Id_A,Area_S,Cat_S,Id_S,Area_1,Cat_1,Id_1,
               Card_A,_,_,_,UML_A,Tipo_A),
    \+member(asociacion(token(Area_A,'AS',Id_A),_,_),Camino),
    \+member(asociacion(_,token(Area_1,Cat_1,Id_1),token(Area_S,Cat_S,Id_S)),
             Camino),
    concepto(Area_S,Cat_S,Id_S,_,Desc_S,_,Nat_S,_,_),
%%  write_sem_set(elemento(token(Area_1,Cat_1,Id_1,Vi_1,Vf_1,Fun_1),
%%                Semantica_1,Pragmatica_1,Lista_1),2),
/*    write_sem_set(elemento(      token(Area_S,Cat_S,Id_S,_Vi_S,_Vf_S,_Fun_S),
		      semantica(_Cuan_S,_Pol_S,Desc_S,Nat_S,_Tipo_S),
		      Pragmatica_1,
		      [
		       asoc(
			    token(Area_A,'AS',Id_A,_Vi_A,_Vf_A,_Fun_A),
			    semantica(UML_A,Tipo_A,Desc_A,Nat_A,Card_A),
			    elemento(
				     token(Area_1,Cat_1,Id_1,Vi_1,Vf_1,Fun_1),
				     Semantica_1,
				     Pragmatica_1,
				     Lista_1))
		       ]),2),
*/
%%    format("***********************************************************~n"),
%%    format("*****************************************************HECHO.~n"),
    elemento(Area_S,Cat_S,Id_S,_,_,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	convert
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Elem ==> Elem
convert(token(_Indice, Area,'EL',Id,Valor_ini,Valor_fin,Funcion),
	elemento(
		token( Area, 'EL', Id, Valor_ini, Valor_fin, Funcion),
		semantica( _Cuant_, _Polaridad_, Desc_Larga, Naturaleza, 
		           _Tipo),
		pragmatica( _Accion, _Descripcion, _Objeto, _Orden, 
                            _Periferico),
		[]
	)):-
	concepto(Area,'EL',Id,_Descr_Corta,Desc_Larga,_Texto,Naturaleza,_,_),
        elemento(Area,'EL',Id,_,_,_),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Value ==> Elem
convert(token(_Indice, Area,'VA',Id,Valor_ini,Valor_fin,Funcion),
	elemento(
	  token(Area_Elem, 'EL', Id_Elem, _Valor_ini, _Valor_fin, Funcion),
	  semantica( _Cuant, _Polaridad, Desc_Larga_Elem, Naturaleza, _Tipo),
	  pragmatica( _Accion, _Descripcion, _Objeto, _Orden, _Peroferico),
	   [
	    asoc(
  	        token( Area, 'AS', 'tiene_valor', _Valor_ini, _Valor_fin, 
	               _Funcion),
  semantica( _Tipo_UML, _Tipo, 'Asociacion atributo <-> valor', _Nat, _Card),
	   elemento(
		   token( Area, 'VA', Id, Valor_ini, Valor_fin, Funcion),
	   semantica(_Cuant, _Pol, Desc_Larga_Val, Naturaleza_Val, Tipo_Dato),
		   pragmatica( _Accion, _Desc, _Objeto, _Orden, _Peroferico),
		   []
		            )  %%elemento
   			)  %%asoc
		   ])
		  ):-
	concepto(Area,'VA',Id,_,Desc_Larga_Val,_,Naturaleza_Val,_,_),
	valor(Area,'VA',Id,Area_Elem,'EL',Id_Elem,_,_,_,_,_,_,Tipo_Dato),
	concepto(Area_Elem,'EL',Id_Elem,_Descr_Corta,Desc_Larga_Elem,
						 _Texto,Naturaleza,_,_),
        elemento(Area_Elem,'EL',Id_Elem,_,_,_),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Asoc ==> Elem
 convert(token( _Indice,Area,'AS',Id,Valor_ini,Valor_fin,Funcion),
	elemento(
		token( Area_Elem_Origen, 'EL', Id_Elem_Origen, _Valor_ini, _Valor_fin_, _Funcion),
		semantica( _Cuant, _Pol, Desc_Larga_Elem_Origen, 
                                  Naturaleza_Elem_Origen, _Tipo),
		pragmatica( Accion, Desc, Objeto, Orden, Perif),
		[
		   asoc(
			   token( Area, 'AS', Id, Valor_ini, Valor_fin, Funcion), 
			   semantica( Tipo_UML, Tipo, Desc_Larga_Asoc, Naturaleza_Asoc, Cardinalidad),
			   elemento(
				   token( Area_Elem_Destino, 'EL', Id_Elem_Destino, _V_ini, _V_fin, _Funcion),
				   semantica( _Cuant, _Pol, Desc_Larga_Elem_Destino, 
                                                    Naturaleza_Elem_Destino, _Tipo),
				   pragmatica( Accion, Desc, Objeto, Orden, Perif),
				   []
			   )  %%elemento
   			)  %%asoc
		   ])
		  ):-
   concepto(Area,'AS',Id,_,Desc_Larga_Asoc,_,Naturaleza_Asoc,_,_),
   asociacion(Area,'AS',Id,Area_Elem_Origen,'EL',Id_Elem_Origen,Area_Elem_Destino,'EL',Id_Elem_Destino,
                                                                  Cardinalidad,_,_,_,Tipo_UML,Tipo),
   concepto(Area_Elem_Origen,'EL',Id_Elem_Origen,_,Desc_Larga_Elem_Origen,_,Naturaleza_Elem_Origen,_,_),
   concepto(Area_Elem_Destino,'EL',Id_Elem_Destino,_,Desc_Larga_Elem_Destino,_,Naturaleza_Elem_Destino,_,_),
   elemento(Area_Elem_Origen,'EL',Id_Elem_Origen,_,_,_), 
   elemento(Area_Elem_Destino,'EL',Id_Elem_Destino,_,_,_),   
   !.
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Accion ==> Elem
convert(token(_Indice, Area,'AC',Id,_Valor_ini,_Valor_fin,_Funcion),
	elemento(
		token( _Area, 'EL', _Id, _Valor_ini, _Valor_fin, _Funcion),
		semantica( _Cuant, _Pol, _Desc, _Nat, _Tipo),
		pragmatica( Id, Desc_Larga, _Objeto, _Orden, _Periferico),
		[]
	)):-
	concepto(Area,'AC',Id,_Descr_Corta,Desc_Larga,_Texto,_Naturaleza,_,_),
   !.	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Objeto ==> Elem
convert(token(_Indice,Area,'OB',Id,_Valor_ini,_Valor_fin,_Funcion),
	elemento(
		token( _Area, 'EL', _Id, _Valor_ini, _Valor_fin, _Funcion),
                semantica( _Cuant, _Pol, _Desc, _Nat, _Tipo),
		pragmatica( _Accion, _Desc, _Objeto, _Orden, _Perif),
			[]
	)):-
	concepto(Area,'OB',Id,_Descr_Corta,_Desc_Larga,_Texto,_Naturaleza,_,_),
   !.	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Orden ==> Elem
convert(token(_Indice,Area,'OR',Id,_Valor_ini,_Valor_fin,_Funcion),
	elemento(
		token( _Area, 'EL', _Id, _Valor_ini, _Valor_fin, _Funcion),
		semantica( _Cuantif, _Pol, _Desc, _Nat, _Tipo),
		pragmatica( _Accion, _Desc, _Objeto, Id, _Perif),
		[]
	)):-
	concepto(Area,'OR',Id,_Descr_Corta,_Desc_Larga,_Texto,_Naturaleza,_,_),
   !.	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Perif ==> Elem
convert(token(_Indice,Area,'PE',Id,_Valor_ini,_Valor_fin,_Funcion),
	elemento(
		token( _Area, 'EL', _Id, _Valor_ini, _Valor_fin, _Funcion),
		semantica( _Cuant, _Pol, _Desc, _Natur, _Tipo),
		pragmatica( _Accion, _Desc, _Objeto, _Orden, Id),
		[]
	)):-
	concepto(Area,'PE',Id,_Descr_Corta,_Desc_Larga,_Texto,_Naturaleza,_,_),
   !.	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Preposiciones ==> Elem

convert(token(_Indice,_Area,'SX','PRPO',_Valor_ini,_Valor_fin,_Funcion),null).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Relativos ==> Elem
   
convert(token(_Indice,_Area,'SX','RELA',_Valor_ini,_Valor_fin,_Funcion),null).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Demostrativo ==> Elem
convert(token(_Indice,Area,'SX','DEMO',_Valor_ini,_Valor_fin,_Funcion),
	elemento(
		token( _Area, 'EL', _Id, _Valor_ini, _Valor_fin, _Funcion),
		semantica( _Cuan, _Pol, _Desc, Naturaleza, _Tipo),
		pragmatica( _Accion, _Desc, _Obj, _Ord, _Perif),
		[]
	)):-
	concepto(Area,'SX','DEMO',_Descr_Corta,_Desc_Larga,_Texto,Naturaleza,_,_),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Articulo + que  ==> Elem
convert(token(_Indice,Area,'SX','ARQU',_Valor_ini,_Valor_fin,_Funcion),
	elemento(
		token( _Area, 'EL', _Id, _Valor_ini, _Valor_fin, _Funcion),
		semantica( _Cuan, _Pol, _Desc, _Naturaleza, _Tipo),
		pragmatica( _Accion, _Desc, _Objeto, _Orden, _Perif),
	        []
	)):-
	concepto(Area,'SX','ARQU',_Descr_Corta,_Desc_Larga,_Texto,_,_,_),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Cuantificador  ==> Elem
convert(token(_Indice,Area,'SX','CTFC',Valor_ini,_Valor_fin,_Funcion),
	elemento(
		token( _Area, 'EL', _Id, _Vi, _Vf, _Funcion),
		semantica( Valor_ini, _Pol, _Desc, _Naturaleza, _Tipo),
		pragmatica( _Accion, _Desc_Accion, _Objeto, _Orden, _Perif),
	        []
	)):-
	concepto(Area,'SX','CTFC',_Descr_Corta,_Desc_Larga,_Texto,_,_,_),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Fallo ==> Fallo
convert(Token,fallo):-
   format("No se ha podido convertir: ~w~n",[Token]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	write_elem
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write_sem_set(elemento( token(Area,Cat,Id,Vi,Vf,F),
                        semantica(Ctx,Pol,D,Nat,Tipo),
                        pragmatica(Acc,DA,Obj,Ord,Perif),
                        Lista_Asoc),Deep):-
	format_slot(Deep,"-------------------------------~n",[]),
        format_slot(Deep,"Area:         ~w~n",[Area]),
	format_slot(Deep,"Categoria:    ~w~n",[Cat]),
	format_slot(Deep,"Id:           ~w~n",[Id]),
	format_slot(Deep,"Valor ini:    ~w~n",[Vi]),
	format_slot(Deep,"Valor fin:    ~w~n",[Vf]),
	format_slot(Deep,"Funcion:      ~w~n",[F]),
  	format_slot(Deep,"Cuantific:    ~w~n",[Ctx]),
	format_slot(Deep,"Polaridad:    ~w~n",[Pol]),
	format_slot(Deep,"Descrip:      ~w~n",[D]),
	format_slot(Deep,"Naturaleza:   ~w~n",[Nat]),
	format_slot(Deep,"Tipo:         ~w~n",[Tipo]),
        format_slot(Deep,"Accion:       ~w~n",[Acc]),
        format_slot(Deep,"Des Accion:   ~w~n",[DA]),
        format_slot(Deep,"Objeto:       ~w~n",[Obj]),
        format_slot(Deep,"Orden:        ~w~n",[Ord]),
        format_slot(Deep,"Periferico:   ~w~n",[Perif]),
	Deep_Next is Deep + 1,
	write_assocs(Lista_Asoc,Deep_Next),
	format_slot(Deep,"====================================~n",[]).

format_slot(Deep,String,Arg):-
	%%ground(Arg),
	!,
	format_deep(Deep),
	format(String,Arg).

format_slot(_,_,_).


write_assocs([],_).
write_assocs([asoc(token(Area,Cat,Id,Vi,Vf,F),
              semantica(Tipo_UML,Tipo,Desc,Nat,Card),Elem)|Tail_Assocs],Deep):-
        format_slot(Deep,"<-------ASOCIACION-------------------~n",[]),
        format_slot(Deep,"<Area:        ~w~n",[Area]),
	format_slot(Deep,"<Categoria:   ~w~n",[Cat]),
	format_slot(Deep,"<Id:          ~w~n",[Id]),
	format_slot(Deep,"<Valor ini:   ~w~n",[Vi]),
	format_slot(Deep,"<Valor fin:   ~w~n",[Vf]),
	format_slot(Deep,"<Funcion:     ~w~n",[F]),
	format_slot(Deep,"<Tipo UML:    ~w~n",[Tipo_UML]),
	format_slot(Deep,"<Tipo:        ~w~n",[Tipo]),
	format_slot(Deep,"<Descrip:     ~w~n",[Desc]),
 	format_slot(Deep,"<Naturaleza:  ~w~n",[Nat]),
 	format_slot(Deep,"<Cardinal:    ~w~n",[Card]),
	write_sem_set(Elem,Deep),
	write_assocs(Tail_Assocs,Deep).
	
write_list([]):-
	format("================Fin Lista=============~n~n~n",[]).
write_list([Elem|Tail]):-
	write_sem_set(Elem,0),
	!,
	write_list(Tail).
	
write_list([Elem|Tail]):-
	format(" Unknown: ~w~n",[Elem]),	
	write_list(Tail).
	
format_deep(0):-
	!.
format_deep(N):-
	format("         ",[]),
	N1 is N - 1,
	format_deep(N1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%        coste
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

coste([]).
coste([asociacion(token(Area_A,Cat_A,Id_A),_,_)|R]):-
     concepto(Area_A,Cat_A,Id_A,_,Desc,_,_,_,_),
     format("Se ha introducido la asociacion: Area: ~w, Categoria: ~w, Id: ~w ==> ~w~n",[Area_A,Cat_A,Id_A,Desc]),
     coste(R).
