%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  value(
%        area:			Area,
%        category:		'VA',
%	 id: 			Identificador,
%	 area_elem: 		Area_Elem,
%	 category_elem:		'EL',
%	 id_elem:		Identificador_Elem,
%	 area_elem_gen:		Area_Elem_Gen,
%	 category_elem_gen:	'EG',
%	 id_elem_gen:		Identificador_Elem_Gen,
%	 area_value_gen:	Area_Valor_Gen,
%	 category_value_gen:	'VG',
%	 id_value_gen:		Identificador_Valor_Gen,
%	 value_type:		Tipo (NU:numero, FX:fecha, HO:hora, 
%					CO:alfnum, TX:texto)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% elem(
%        area:			Area,
%        category:		'EL',
%	 id: 			Identificador,
%	 area_elem_gen:		Area_Elem_Gen,
%	 category_elem_gen:	'EG',
%	 id_elem_gen:		Identificador_Elem_Gen)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% assoc(
%	 area:			Area,
%	 category: 		'AS',
%	 id:			Identificador,
%	 area_elem_orig:	Area_Elemento_Origen,
%	 category_elem_orig:	'EL',
%	 id_elem_orig:		Identificador_Elemento_Origen,
%	 area_elem_dest:	Area_Elemento_Destino,
%	 category_elem_dest:	'EL',
%	 id_elem_dest:		Identificador_Elemento_Destino,
%	 cardinal:		Cardinalidad en el destino: 01,11,0N,1N,NN
%	 area_assoc_gen:	Area_Assoc_Gen,
%	 category_assoc_gen:	'AG',
%	 id_assoc_gen:		Identificador Asociacion Generalizada,
%        uml_type:		Tipo UML (C:Composicion, 
%					  A:Asociacion,
%					  G:Agregacion)
%	 desc_type:		Tipo Descriptivo (C: Identificador pricipal,
%						  I: Identificador Alternativo,
%						  D: Descripcion del elemento)
%
