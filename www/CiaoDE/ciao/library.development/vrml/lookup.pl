:- module(lookup, [
	create_proto_element/3,
	get_prototype_interface/2,
	get_prototype_definition/2,
	%lookup_eventIn/3,
	%lookup_eventOut/3,
	%%%%%%%%%%%%%%%% DCG
	lookup_check_node/4,
	lookup_check_field/6,
	lookup_check_interface_fieldValue/8,
	%%%%%%%%%%%%%%%%
	lookup_field/4,
	lookup_route/5,
	lookup_fieldTypeId/1,
	lookup_get_fieldType/4,
	lookup_field_access/4,
	lookup_set_def/3,
	lookup_set_prototype/4,
	lookup_set_extern_prototype/4],[dcg]).

:- include(library(assertions)).
:- include(library(basicmodes)).
:- include(library(types)).

:- include(library(iso)).
:- use_module(library(iso_byte_char)).
:- use_module(library(basicprops)).
:- use_module(library(lists)).
:- use_module(error).
:- use_module(io).
:- use_module(parser_util).
:- use_module(dictionary).
:- use_module(dictionary_tree).
:- use_module(field_value_check,[fieldValue_check/8]).
:- use_module(boundary,[boundary_check/3,
	                reserved_words/1,
			children_nodes/1]).
:- use_module(generator_util,[decompose_field/3,
	                      indentation_list/2]).
:- use_module(field_type,[fieldType/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lookup_check_field(In,Out,NodeTypeId,Field) -->
	{
	%write(NodeTypeId),nl,write(Field),nl,
	decompose_field(Field,FieldId,Guts),
	lookup_get_fieldType(In,NodeTypeId,FieldId,FieldType)
	},
	( { FieldType == undefined }
	-> { error_vrml(fieldType_undefined(NodeTypeId,FieldId)) }
	;  { lookup_get_boundary(NodeTypeId, FieldId, InitValue, Boundaries),
	     indentation_list(In,Indent)
	   },
	   ( ( { FieldType == 'SFNode'
	       ; FieldType == 'MFNode' }
	     )
	   -> out(Indent),
	      out([FieldId,' '])
	   ;  out(Indent),
	      out([FieldId,' '])
	   ), 
	   (  { FieldId == children }
	   -> { inc_indentation(In,In0),
	        indentation_list(In,NodeIndent) },
		out(['\n']),
		out(NodeIndent),
		fieldValue_check(FieldType,Guts,In0,In1,InitValue,Boundaries),
	        { dec_indentation(In1,Out) }
	   ;  fieldValue_check(FieldType,Guts,In,Out,InitValue,Boundaries)
	   )
	).
	  
	
lookup_check_node(In,NodeTypeId) -->
	{
	dictionary(NodeTypeId, _FieldAccess, _FieldType, 
	                   _FieldId, _InitValue, _Boundaries),
	children_nodes(CN)
	},
	( { member(NodeTypeId,CN) }
	->{ indentation_list(In,Indent_list) },
	  out(Indent_list),
	  out([NodeTypeId,' {\n'])
	; out([NodeTypeId,' {\n'])
	),
	!.

lookup_check_node(In,NodeTypeId) -->
	{
	get_dictionaries(In,Dic),
	get_prototype_dictionary(Dic,Proto_dic),
	dictionary_lookup(NodeTypeId,'PROTO',_Info,Proto_dic,Status)
	},
	( {Status == defined }
	-> { indentation_list(In,Indent_list) },
	   out(Indent_list),
	   out([NodeTypeId,' {\n'])
	;
	   { dictionary_lookup(NodeTypeId,'DEF',_Info,Proto_dic,Status0) },
	   ( { Status0 == defined }
	   -> { indentation_list(In,Indent_list) },
	      out(Indent_list),
	      out([NodeTypeId,' {\n'])
	   )
	).


lookup_check_node(_In,NodeTypeId) -->
	{ error_vrml(nodeTypeId(NodeTypeId)) }.


%%%%%%%%%%%%%%%%
lookup_check_interface_fieldValue(In,Out,AccessType,FieldType,Id,FieldValue)-->
	{
	strip_clean(FieldType,FieldType_clean),
	indentation_list(In,Indent) 
	},
        out(Indent),
	out([AccessType,' ',FieldType,' ',Id,' ']),
        fieldValue_check(FieldType_clean,FieldValue,In,Out,_InitValue,[]),
	out(['\n']).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lookup_fields(In,NodeTypeId,FieldAccess,FieldType,FieldId,Init,Bound) :-
	lookup_get_fieldType(In,NodeTypeId,FieldId,FieldType),
	( FieldType == undefined
	-> true
	;  lookup_get_boundary(NodeTypeId,FieldId,Init,Bound),
	   lookup_access(In,NodeTypeId,FieldId,FieldAccess)
	).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lookup_get_boundary(NodeTypeId, FieldId, InitValue, Boundaries) :-
	dictionary(NodeTypeId, _FieldAccess, _FieldType, 
	                   FieldId, InitValue, Boundaries).

%We call it only if we find the field, if not in ordinary dictionary
%then it is a personal field without bound.
lookup_get_boundary(_NodeTypeId, _FieldId, _InitValue,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lookup_get_fieldType(_Parse_struct, NodeTypeId, FieldId, FieldType) :-
	dictionary(NodeTypeId, _FieldAccess, FieldType, 
	                   FieldId, _InitValue, _Boundaries),
	!.

lookup_get_fieldType(Parse_struct, NodeTypeId, FieldId, FieldType) :-
	get_dictionaries(Parse_struct,Dic),
	get_prototype_dictionary(Dic,Proto_dic),
	dictionary_lookup(NodeTypeId,'PROTO',Info,Proto_dic,Status),
	Status == defined,
	lookup_fieldType_defined(NodeTypeId,FieldId,FieldType,Info).
	
	
lookup_get_fieldType(Parse_struct, NodeTypeId, FieldId, FieldType) :-
	get_dictionaries(Parse_struct,Dic),
	get_prototype_dictionary(Dic,Proto_dic),
	dictionary_lookup(NodeTypeId,'DEF',Info,Proto_dic,Status),
	Status == defined,
	get_node_name(Info,Name),
	( Name == 'Script'
	-> write('OOOOOOOOOOOOOOOOOO '),nl,write(FieldId),nl,
	   get_node_guts(Info,Interface),write(Interface),nl,
	   strip_from_list(Interface,[Inter_Str]),
	   lookup_interface(Inter_Str,_Acc,FieldType,FieldId,StatInter),
	   ( StatInter == defined
	   -> true
	   ;  error_vrml(fieldType_undefined(NodeTypeId,FieldId))
	   )
	; write('VVVVVAAAAAADDDDDDD FFFFAAAN '),write(Name),nl,
	    dictionary(Name, _FieldAccess, FieldType, 
	                   FieldId, _InitValue, _Boundaries)
	).

lookup_get_fieldType(Parse_struct, NodeTypeId, FieldId, FieldType) :-
	get_dictionaries(Parse_struct,Dic),
	get_prototype_dictionary(Dic,Proto_dic),
	dictionary_lookup(NodeTypeId,'EXTERNPROTO',Info,Proto_dic,Status),
	Status == defined,
	lookup_fieldType_defined(NodeTypeId,FieldId,FieldType,Info).

lookup_get_fieldType(_Parse, _NodeTypeId, _FieldId, undefined).

lookup_fieldType_defined(_NodeTypeId,FieldId,FieldType,Info) :-
	get_prototype_interface(Info,Interface),
	lookup_interface(Interface,_Access,FieldType,FieldId,Status_interface),
	Status_interface == defined.

lookup_fieldType_defined(NodeTypeId,FieldId,_FieldType,_Info) :-
	error_vrml(fieldType_undefined(NodeTypeId,FieldId)),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lookup_field(In,NodeTypeId0, FieldId0,FieldId1) :-
	dictionary(NodeTypeId0, _, FieldType0,
	            FieldId0, _InitValue, _Boundaries), 
	get_environment_name(In,NodeTypeId1),
	lookup_get_fieldType(In,NodeTypeId1,FieldId1,FieldType1),
	( FieldType1 == undefined
	->error_vrml(fieldType_undefined(NodeTypeId1,FieldId1))
	; FieldType0 == FieldType1
	).

lookup_field(In,NodeTypeId0, FieldId0, FieldId1) :-
	get_environment_name(In,NodeTypeId1),
	error_vrml(fieldType(NodeTypeId0,FieldId0,NodeTypeId1,FieldId1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lookup_route(In,NodeTypeId0,FieldId0,NodeTypeId1,FieldId1) :-
	lookup_fields(In,NodeTypeId0,Acc0,Type0,FieldId0,_Init,_Bound),
	( Type0 == undefined
	-> change_name(changed,FieldId0,OutId),
	   lookup_fields(In,NodeTypeId0,Acc0,Type00,OutId,_Init,_Bound),
	   ( Type00 == undefined
	   ->error_vrml(fieldType_undefined(NodeTypeId1,FieldId1))
	   ; Type0_check = Type00
	   )
	;  Type0_check = Type0
	),
	lookup_fields(In,NodeTypeId1,Acc1,Type1,FieldId1,_Init,_Bound),
	( Type1 == undefined
	-> change_name(set,FieldId1,InId),
	   lookup_fields(In,NodeTypeId1,Acc1,Type11,InId,_Init,_Bound),
	   ( Type11 == undefined
	   ->error_vrml(fieldType_undefined(NodeTypeId1,FieldId1))
	   ; Type1_check = Type11
	   )
	; Type1_check = Type1
	),
	( ( Acc0 == eventOut
	  ; Acc0 == exposedField
	  )
	-> true
	;  error_vrml(route_eventOut(NodeTypeId0,FieldId0))
	),
	( ( Acc1 == eventIn		   
	  ; Acc1 == exposedField
	  )
	-> true
	;  error_vrml(route_eventIn(NodeTypeId1,FieldId1))
	),
	( Type0_check == Type1_check
	-> true
	;  error_vrml(route_type(NodeTypeId0,Type0_check,NodeTypeId1,Type1_check))
	).

lookup_route(_In,NodeTypeId0,FieldId0,NodeTypeId1,FieldId1) :-
	error_vrml(route(NodeTypeId0,FieldId0,NodeTypeId1,FieldId1)).

change_name(set,Name,NewName) :-
	(  sub_atom(Name,0,4,_,'set_')
	-> sub_atom(Name,4,_,0,NewName)
	;  atom_concat('set_',Name,NewName)
	).

change_name(changed,Name,NewName) :-
	(  sub_atom(Name,_,8,0,'_changed')
	-> sub_atom(Name,0,8,_,NewName)
	;  atom_concat(Name,'_changed',NewName)
	).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%lookup_eventIn(NodeTypeId, EventInId0,EventInId1) :-
%	dictionary(NodeTypeId, eventIn, FieldType0, 
%	                   EventInId0, _InitValue, _Boundaries),
%	dictionary(NodeTypeId, eventIn, FieldType1, 
%	                   EventInId1, _InitValue, _Boundaries),
%	(FieldType0 \== FieldType1
%	->
%	error_vrml(fieldType(NodeTypeId,EventInId0,FieldType0,
%	                                EventInId1,FieldType1))).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%lookup_eventOut(NodeTypeId, EventOutId0,EventOutId1) :-
%	dictionary(NodeTypeId, eventOut, FieldType0, 
%	                   EventOutId0, _InitValue, _Boundaries),
%	dictionary(NodeTypeId, eventOut, FieldType1, 
%	                   EventOutId1, _InitValue, _Boundaries),
%	(FieldType0 \== FieldType1
%	->
%	error_vrml(fieldType(NodeTypeId,EventOutId0,FieldType0,
%	                                EventOutId1,FieldType1))).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lookup_fieldTypeId(FieldTypeId) :-
	fieldType(FieldTypeId),
	!.

lookup_fieldTypeId(FieldTypeId) :-
	error_vrml(fieldTypeId_undefined(FieldTypeId)),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lookup_use_field.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lookup_use_node.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lookup_set_prototype(In,Name,Interface,Prototype) :-
        strip_interface(Interface,Interface_stripped),
	create_proto_element(Interface_stripped,Prototype,Element),
	get_dictionaries(In,Dic),
	get_prototype_dictionary(Dic,Proto_dic),
	dictionary_insert(Name,'PROTO',Element,Proto_dic,Info),
	(Info == new
	->
	true
	;
	Info == multiple,
	error_vrml(multiple_def(Name))).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lookup_set_extern_prototype(In,Name,Interface,Strings) :-
	strip_interface(Interface,Interface_stripped),
	create_proto_element(Interface_stripped,Strings,Element),
	get_dictionaries(In,Dic),
	get_prototype_dictionary(Dic,Proto_dic),
	dictionary_insert(Name,'EXTERNPROTO',Element,Proto_dic,Info),
	(Info == new
	->
	true
	;
	Info == multiple,
	error_vrml(multiple_def(Name))).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lookup_def_field.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lookup_set_def(In,NodeName,Node) :-
	get_dictionaries(In,Dic),
	get_definition_dictionary(Dic,Def_dic),
	dictionary_insert(NodeName,'DEF',Node,Def_dic,Info),
	(Info == new
	->
	true
	;
	Info == multiple,
	error_vrml(multiple_def(NodeName))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lookup_field_access(Parse_struct, NodeName0, FieldId0, FieldId1) :-
	%write('lookup_field_access/4 PROTO '),write(NodeName0),nl,
	get_dictionaries(Parse_struct,Dic),
	get_prototype_dictionary(Dic,Proto_dic),
	get_environment(Parse_struct,Env),
	get_environment_name(Env,NodeName1),
	dictionary(NodeName0,Acc0,FieldType0,FieldId0,_Init,_Bound),
	dictionary_lookup(NodeName1,'PROTO',Info,Proto_dic,Status),
	(Status == defined
	->
	get_prototype_interface(Info,Interface),
	lookup_interface(Interface,Acc1,FieldType1,FieldId1,Status_interface),
	Status_interface == defined,
	( Acc0 == Acc1
	; Acc0 == exposedField
	),
	FieldType0 == FieldType1
	; error_vrml(accessType(NodeName0,FieldId0,Acc0,
	  NodeName1,FieldId1,Acc1))
	).

lookup_field_access(Parse_struct, NodeName0, FieldId0, FieldId1) :-
	%write('lookup_field_access/4 DEF '),write(NodeName0),nl,
	get_dictionaries(Parse_struct,Dic),
	get_prototype_dictionary(Dic,Proto_dic),
	get_environment(Parse_struct,Env),
	get_environment_name(Env,NodeName1),
	dictionary(NodeName0,Acc0,FieldType0,FieldId0,_Init,_Bound),
	dictionary_lookup(NodeName1,'DEF',Info,Proto_dic,Status),
	  (Status == defined
	  ->
	  strip_interface(Info,[Interface]),
	  lookup_interface(Interface,Acc1,FieldType1,FieldId1,Status_interface),
	  Status_interface == defined,
	  ( Acc0 == Acc1
	  ; Acc0 == exposedField
	  ),
	  FieldType0 == FieldType1
	  ; error_vrml(accessType(NodeName0,FieldId0,Acc0,
	    NodeName1,FieldId1,Acc1))
	  ).


lookup_field_access(Parse_struct, NodeName0, FieldId0, FieldId1) :-
	%write('lookup_field_access/4 EXTERNPROTO'),write(NodeName0),nl,
	get_dictionaries(Parse_struct,Dic),
	get_prototype_dictionary(Dic,Proto_dic),
	get_environment(Parse_struct,Env),
	get_environment_name(Env,NodeName1),
	dictionary(NodeName0,Acc0,FieldType0,FieldId0,_Init,_Bound),
	dictionary_lookup(NodeName1,'EXTERNPROTO',Info,Proto_dic,Status),
	  (  Status == defined
	  -> get_prototype_interface(Info,Interface),
	     lookup_interface(Interface,Acc1,FieldType1,FieldId1,Status_interface),
	     Status_interface == defined,
	     ( Acc0 == Acc1
	     ; Acc0 == exposedField
	     ),
	     FieldType0 == FieldType1
	  ;  error_vrml(accessType(NodeName0,FieldId0,Acc0,
	                      NodeName1,FieldId1,Acc1))
	).

lookup_field_access(In, NodeName0, FieldId0, FieldId1) :-
	get_environment(In,Env),
	get_environment_name(Env,NodeName1),
	error_vrml(fieldType(NodeName0,FieldId0,NodeName1,FieldId1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lookup_access(_In,NodeTypeId,FieldId,Acc) :-
	dictionary(NodeTypeId,Acc,_FieldType,FieldId,_Init,_Bound).

lookup_access(In,NodeTypeId,FieldId,Acc) :-
	write('lookup_access/4 PROTO'),nl,
	get_dictionaries(In,Dic),
	get_prototype_dictionary(Dic,Proto_dic),
	dictionary_lookup(NodeTypeId,'PROTO',Info,Proto_dic,Status),
	(Status == defined
	->
	get_prototype_interface(Info,Interface),
	lookup_interface(Interface,Acc,_FieldType,FieldId,Status_interface),
	Status_interface == defined
	).

lookup_access(In, NodeTypeId, FieldId, Acc) :-
	write('lookup_access/4 DEF'),nl,
	get_dictionaries(In,Dic),
	get_prototype_dictionary(Dic,Proto_dic),
	dictionary_lookup(NodeTypeId,'DEF',Info,Proto_dic,Status),
	Status == defined,
	get_node_name(Info,Name),write(Info),nl,
	( Name == 'Script'
	-> get_node_guts(Info,Interface),write(Interface),nl,
	   strip_from_list(Interface,[Inter_Str]),
	   lookup_interface(Inter_Str,Acc,_FieldType,FieldId,StatInter),
	   ( StatInter == defined
	   -> true
	   ;  error_vrml(fieldType_undefined(NodeTypeId,FieldId))
	   )
	;
	dictionary(Name, Acc, _FieldType, 
	                   FieldId, _InitValue, _Boundaries)
	).

lookup_access(In,NodeTypeId,FieldId,Acc) :-
	get_dictionaries(In,Dic),
	get_prototype_dictionary(Dic,Proto_dic),
	dictionary_lookup(NodeTypeId,'EXTERNPROTO',Info,Proto_dic,Status),
	(Status == defined
	->
	get_prototype_interface(Info,Interface),
	lookup_interface(Interface,Acc,_FieldType,FieldId,Status_interface),
	Status_interface == defined
	).


        


lookup_access(_In,NodeTypeId,FieldId,_Acc) :-
	error_vrml(accessType(NodeTypeId,FieldId)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lookup_interface([], _Acc,_Type,_Id, undefined).
lookup_interface([Node|_Interface], Acc, Type, Id, Stat) :-
	Node =.. [Acc,Type,Id|_More],
	Stat = defined.

lookup_interface([_Node|Interface], Acc,Type,Id, Stat) :-
	lookup_interface(Interface,Acc,Type,Id,Stat).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lookup_event_list([], _Acc,_Type,_Id, undefined).
lookup_event_list([Node|_Interface], Acc, Type, Id, Stat) :-
	Node =.. [Acc,Type,Id],
	Stat = defined.

lookup_event_list([_Node|Interface], Acc,Type,Id, Stat) :-
	lookup_event_list(Interface,Acc,Type,Id,Stat).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_proto_element(Interface,Definition,proto(Interface,Definition)).
get_prototype_interface(proto(Interface,_Definition),Interface).
get_prototype_definition(proto(_Interface,Definition),Definition).
	
get_node_name([Node],Name) :-
	Node =.. [Name|_Guts].

get_node_guts([Node],Guts) :-
	Node =.. [_Name|Guts].
