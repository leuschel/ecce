:-module(convert,[convert/2]). 

:-consult(con_concepto).
:-consult(aso_asociacion).
:-consult(val_valor).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	convert
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Elem ==> Elem
convert(token(Area,'EL',Id,Valor_ini,Valor_fin,Funcion),
	elemento(
		token(
		        area:              Area,
			categoria:	   'EL',
			id:		      Id,
			valor_ini:	   Valor_ini,
			valor_fin:	   Valor_fin,
			funcion:	      Funcion
		),
		semantica(
			cuantificador:	_,
			polaridad:     _,
			descripcion:	Desc_Larga,
			naturaleza:	  Naturaleza,
			tipo:		 _
		),
		pragmatica(
			accion:		   _,
			descripcion:   _,
			 objeto:        _,
			 orden:         _,
			 periferico:    _
		),
		[]
	)):-
	concepto(Area,'EL',Id,_Descr_Corta,Desc_Larga,_Texto,Naturaleza,_,_),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Value ==> Elem
convert(token(Area,'VA',Id,Valor_ini,Valor_fin,Funcion),
	elemento(
		token(
		        area:              Area_Elem,
			categoria:	   'EL',
			id:		      Id_Elem,
			valor_ini:	   _,
			valor_fin:	   _,
			funcion:	      Funcion
		),
		semantica(
			cuantificador:	_,
			polaridad:     _,
			descripcion:	Desc_Larga_Elem,
			naturaleza: 	Naturaleza,
			tipo:		      _
		),
		pragmatica(
			accion:		   _,
			descripcion:   _,
         objeto:        _,
         orden:         _,
         periferico:    _
		),
		[
		   asoc(
			   token(
			           area:       Area,
				   categoria:	'AS',
				   id:	   	'tiene_valor',
				   valor_ini:	_,
				   valor_fin:	_,
				   funcion: 	_
			   ),
			   semantica(
				   tipo_uml:	_,
				   tipo: 		_,
               descripcion:   'Asociacion atributo <-> valor', 
               naturaleza:    _,
               cardinalidad:  _
			   ),
			   elemento(
				   token(
				           area:              Area,
					   categoria:	   'VA',
					   id:	   	   Id,
					   valor_ini:	   Valor_ini,
					   valor_fin:	   Valor_fin,
					   funcion:	      Funcion
				   ),
				   semantica(
					   cuantificador:	_,
					   polaridad:     _,
					   descripcion:	Desc_Larga_Val,
					   naturaleza:	   Naturaleza_Val,
					   tipo:		      Tipo_Dato
				   ),
				   pragmatica(
					   accion:        _,
					   descripcion:   _,
					   objeto:        _,
					   orden:         _,
					   periferico:    _
				  ),
				  []
			   )  %%elemento
   			)  %%asoc
		   ])
		  ):-
	concepto(Area,'VA',Id,_,Desc_Larga_Val,_,Naturaleza_Val,_,_),
	valor(Area,'VA',Id,Area_Elem,'EL',Id_Elem,_,_,_,_,_,_,Tipo_Dato),
	concepto(Area_Elem,'EL',Id_Elem,_Descr_Corta,Desc_Larga_Elem,
						 _Texto,Naturaleza,_,_),
	 !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Asoc ==> Elem
 convert(token(Area,'AS',Id,Valor_ini,Valor_fin,Funcion),
	elemento(
		token(
		        area:              Area_Elem_Origen,
			categoria:	   'EL',
			id:		   Id_Elem_Origen,
			valor_ini:	   _,
			valor_fin:	   _,
			funcion:	    _
		),
		semantica(
			cuantificador:	_,
			polaridad:     _,
			descripcion:	Desc_Larga_Elem_Origen,
			naturaleza: 	Naturaleza_Elem_Origen,
			tipo:		      _
		),
		pragmatica(
			accion:		   _,
			descripcion:   _,
         objeto:        _,
         orden:         _,
         periferico:    _
		),
		[
		   asoc(
			   token(
			           area:              Area,
				   categoria:	'AS',
				   id:	   	Id,
				   valor_ini:	Valor_ini,
				   valor_fin:	Valor_fin,
				   funcion: 	Funcion
			   ),
			   semantica(
				   tipo_uml:	   Tipo_UML,
				   tipo: 		   Tipo,
				   descripcion:   Desc_Larga_Asoc,
				   naturaleza:    Naturaleza_Asoc,
				   cardinalidad:  Cardinalidad
			   ),
			   elemento(
				   token(
				           area:           Area_Elem_Destino,
					   categoria:	   'EL',
					   id:	   	   Id_Elem_Destino,
					   valor_ini:	   _,
					   valor_fin:	   _,
					   funcion:	      _
				   ),
				   semantica(
					   cuantificador:	_,
					   polaridad:     _,
					   descripcion:	Desc_Larga_Elem_Destino,
					   naturaleza:	   Naturaleza_Elem_Destino,
					   tipo:		      _
				   ),
				   pragmatica(
					   accion:		   _,
					   descripcion:   _,
					   objeto:        _,
					   orden:         _,
					   periferico:	   _
	   			),
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
   !.
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Accion ==> Elem
convert(token(Area,'AC',Id,_Valor_ini,_Valor_fin,_Funcion),
	elemento(
		token(
		        area:              _,  
			categoria:	   'EL',
			id:		      _,
			valor_ini:	   _,
			valor_fin:	   _,
			funcion:	      _
		),
		semantica(
			cuantificador:	_,
			polaridad:     _,
			descripcion:	_,
			naturaleza:	   _,
			tipo:		      _
		),
		pragmatica(
			accion:		   Id,
			descripcion:   Desc_Larga,
			 objeto:        _,
			 orden:         _,
			 periferico:    _
		),
		[]
	)):-
	concepto(Area,'AC',Id,_Descr_Corta,Desc_Larga,_Texto,_Naturaleza,_,_),
   !.	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Objeto ==> Elem
convert(token(Area,'OB',Id,_Valor_ini,_Valor_fin,_Funcion),
	elemento(
		token(
		        area:             _,
			categoria:	   'EL',
			id:		      _,
			valor_ini:	   _,
			valor_fin:	   _,
			funcion:	      _
		),
		semantica(
			cuantificador:	_,
			polaridad:     _,
			descripcion:	_,
			naturaleza:	   _,
			tipo:		      _
		),
		pragmatica(
			accion:		   _,
			descripcion:   _,
			 objeto:        Id,
			 orden:         _,
			 periferico:    _
		),
		[]
	)):-
	concepto(Area,'OB',Id,_Descr_Corta,_Desc_Larga,_Texto,_Naturaleza,_,_),
   !.	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Orden ==> Elem
convert(token(Area,'OR',Id,_Valor_ini,_Valor_fin,_Funcion),
	elemento(
		token(
		        area:              _, 
			categoria:	   'EL',
			id:		      _,
			valor_ini:	   _,
			valor_fin:	   _,
			funcion:	      _
		),
		semantica(
			cuantificador:	_,
			polaridad:     _,
			descripcion:	_,
			naturaleza:	   _,
			tipo:		      _
		),
		pragmatica(
			accion:		   _,
			descripcion:   _,
         objeto:        _,
         orden:         Id,
         periferico:    _
		),
		[]
	)):-
	concepto(Area,'OR',Id,_Descr_Corta,_Desc_Larga,_Texto,_Naturaleza,_,_),
   !.	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Perif ==> Elem
convert(token(Area,'PE',Id,_Valor_ini,_Valor_fin,_Funcion),
	elemento(
		token(
		        area:          _,
			categoria:	   'EL',
			id:		      _,
			valor_ini:	   _,
			valor_fin:	   _,
			funcion:	      _
		),
		semantica(
			cuantificador:	_,
			polaridad:     _,
			descripcion:	_,
			naturaleza:	   _,
			tipo:		      _
		),
		pragmatica(
			accion:		   _,
			descripcion:   _,
			 objeto:        _,
			 orden:         _,
			 periferico:    Id
		),
		[]
	)):-
	concepto(Area,'PE',Id,_Descr_Corta,_Desc_Larga,_Texto,_Naturaleza,_,_),
   !.	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Preposiciones ==> Elem
/*
convert(token(Area,'SX','PRPO',_Valor_ini,_Valor_fin,_Funcion),
	elemento(
		token(
	  	        area:             _,
			categoria:	   'EL',
			id:		      _,
			valor_ini:	   _,
			valor_fin:	   _,
			funcion:	      _
		),
		semantica(
			cuantificador:	_,
			polaridad:     _,
			descripcion:	_,
			naturaleza: 	_,
			tipo:		      _
		),
		pragmatica(
			accion:		   _,
			descripcion:   _,
         objeto:        _,
         orden:         _,
         periferico:    _
		),
		[
		   asoc(
			   token(
			       area:              _,
				   categoria:	'AS',
				   id:	   	_,
				   valor_ini:	_,
				   valor_fin:	_,
				   funcion: 	_
			   ),
			   semantica(
				   tipo_uml:	   _,
				   tipo: 		   _,
				   descripcion:   _,
				   naturaleza:    _,
				   cardinalidad:  _
			   ),
			   elemento(
				   token(
				       area:           _,
					   categoria:	   'EL',
					   id:	   	   _,
					   valor_ini:	   _,
					   valor_fin:	   _,
					   funcion:	      _
				   ),
				   semantica(
					   cuantificador:	_,
					   polaridad:     _,
					   descripcion:	_,
					   naturaleza:	   _,
					   tipo:		      _
				   ),
				   pragmatica(
					   accion:		   _,
					    descripcion:   _,
					    objeto:        _,
					    orden:         _,
   					periferico:	   _
	   			),
				   []
			   )  %%elemento
   			)  %%asoc
		   ])
		  ):-
	concepto(Area,'SX','PRPO',_,_Desc_Larga_Asoc,_,_Naturaleza,_,_),
   !.
 */ 
convert(token(_Area,'SX','PRPO',_Valor_ini,_Valor_fin,_Funcion),null).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Relativos ==> Elem
/*
convert(token(Area,'SX','RELA',_Valor_ini,_Valor_fin,_Funcion),
	elemento(
		token(
		        area:               _,
			categoria:	   'EL',
			id:		      _,
			valor_ini:	   _,
			valor_fin:	   _,
			funcion:	      _
		),
		semantica(
			cuantificador:	_,
			polaridad:     _,
			descripcion:	_,
			naturaleza: 	_,
			tipo:		      _
		),
		pragmatica(
			accion:		   _,
			descripcion:   _,
         objeto:        _,
         orden:         _,
         periferico:    _
		),
		[
		   asoc(
			   token(
			       area:        _,
				   categoria:	'AS',
				   id:	   	_,
				   valor_ini:	_,
				   valor_fin:	_,
				   funcion: 	_
			   ),
			   semantica(
				   tipo_uml:	   _,
				   tipo: 		   _,
				   descripcion:   _,
				   naturaleza:    _,
				   cardinalidad:  _
			   ),
			   elemento(
				   token(
				       area:        _,
				           categoria:	   'EL',
					   id:	   	   _,
					   valor_ini:	   _,
					   valor_fin:	   _,
					   funcion:	      _
				   ),
				   semantica(
					   cuantificador:	_,
					   polaridad:     _,
					   descripcion:	_,
					   naturaleza:	   _,
					   tipo:		      _
				   ),
				   pragmatica(
					   accion:		   _,
					    descripcion:   _,
					    objeto:        _,
					    orden:         _,
   					periferico:	   _
	   			),
				   []
			   )  %%elemento
   			)  %%asoc
		   ])
		  ):-
	concepto(Area,'SX','RELA',_,_Desc_Larga_Asoc,_,_Naturaleza_Asoc,_,_),
   !.
*/
   
convert(token(_Area,'SX','RELA',_Valor_ini,_Valor_fin,_Funcion),null).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Demostrativo ==> Elem
convert(token(Area,'SX','DEMO',_Valor_ini,_Valor_fin,_Funcion),
	elemento(
		token(
		    area:           _,
			categoria:	   'EL',
			id:		      _,
			valor_ini:	   _,
			valor_fin:	   _,
			funcion:	      _
		),
		semantica(
			cuantificador:	_,
			polaridad:     _,
			descripcion:	_,
			naturaleza:	   Naturaleza,
			tipo:		      _
		),
		pragmatica(
			accion:		   _,
			descripcion:   _,
			 objeto:        _,
			 orden:         _,
			 periferico:    _
		),
		[]
	)):-
	concepto(Area,'SX','DEMO',_Descr_Corta,_Desc_Larga,_Texto,Naturaleza,_,_),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Articulo + que  ==> Elem
convert(token(Area,'SX','ARQU',_Valor_ini,_Valor_fin,_Funcion),
	elemento(
		token(
		    area:            _,
			categoria:	   'EL',
			id:		      _,
			valor_ini:	   _,
			valor_fin:	   _,
			funcion:	      _
		),
		semantica(
			cuantificador:	_,
			polaridad:     _,
			descripcion:	_,
			naturaleza:	   _,
			tipo:		      _
		),
		pragmatica(
			accion:		   _,
			descripcion:   _,
			 objeto:        _,
			 orden:         _,
			 periferico:    _
		),
		[]
	)):-
	concepto(Area,'SX','ARQU',_Descr_Corta,_Desc_Larga,_Texto,_,_,_),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Fallo ==> Fallo
convert(Token,fallo):-
   format("No se ha podido convertir: ~w~n",[Token]).
