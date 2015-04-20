:- module(parser, [parser/2,nodeDeclaration/4,comment/4], [dcg]).

:- include(library(assertions)).
:- include(library(basicmodes)).
:- include(library(types)).

:- include(library(iso)).
:- use_module(library(iso_byte_char)).
:- use_module(library(basicprops)).
:- use_module(library(lists)).
:- use_module(lookup).
:- use_module(field_value).
:- use_module(tokeniser).
:- use_module(parser_util).
:- use_module(error).

%:- set_prolog_flag(multi_arity_warnings, off).
%%%:- use_module(internal_types).
%%%:- use_module(i_o).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parser(VRML, Terms) :-
	tokeniser(VRML, Tokens),
	create_parse_structure(Parse),
       	vrmlScene_first(Parse, Out, Tokens, []),
	reverse_parsed(Out,Parse_out),
	get_parsed(Parse_out,Terms).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
vrmlScene_first(In,Out) -->
	header(In,Out0),
	vrmlScene(Out0,Out).

vrmlScene(In,Out) -->
	declarations(In, Out).


	
%We read comment('#VRML V2.0 utf8') and the following
header(In,Out) -->
	[comment(Header)],
	{name(Header, Header_list),
	 Header_list = [35,86,82,77,76,32,86,50,46,48,32,117,116,102,56|_More],
	 insert_parsed([Header],In,Out)}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
declarations(In,Out) -->
	[],
	{stop_parse(In,Out) }.

declarations(In,Out) -->
	declaration(In,Out0),
	declarations(Out0,Out).

%%%%%%%%%%%%%%%%
declaration(In,Out) -->
	[id('NULL')],
	 {insert_parsed(['NULL'],In, Out)}.

declaration(In, Out) -->
	nodeDeclaration(In,Out).

declaration(In, Out) -->
	protoDeclaration(In, Out).

declaration(In, Out) -->
	routeDeclaration(In, Out).

declaration(In,Out) -->
	comment(In,In0),
	declaration(In0,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nodeDeclaration(In,Out) -->
	[id('DEF')],
	 comment(In,In0),
	nodeNameId(NodeNameId),
	{create_parse_structure(P)},
	node(P,Node_parsed),
	% DEF inside a PROTO cannot be accessed from outside 
	% DEF outside a PROTO cannot be accessed from a PROTO
	{get_parsed(Node_parsed,Node),
	 lookup_set_def(In0,NodeNameId,Node),
	 insert_parsed(['DEF'(NodeNameId,Node)],In0,Out)}.

nodeDeclaration(In,Out) -->
	[id('USE')],
	 comment(In,In0),
	nodeNameId(NodeNameId),
	% is the nodeNameId possible to access inside/outside
	{insert_parsed(['USE'(NodeNameId)],In0,Out)}.

nodeDeclaration(In,Out) -->
	node(In,Out).

nodeDeclaration(In,Out) -->
	comment(In,In0),
	nodeDeclaration(In0,Out).

nodeDeclaration(In,Out) -->
	[],
	{stop_parse(In,Out) }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
protoDeclaration(In, Out) -->
	proto(In, Out).

protoDeclaration(In, Out) -->
	externproto(In, Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
proto(In, Out) -->
	[id('PROTO')],
	 nodeTypeId(NodeTypeId),
	[parenthesis_list_open],
	{create_parse_structure(P0)},
	interfaceDeclarations(P0,InterfaceDecl_parsed),
	[parenthesis_list_close],
	 % Set proto entry in a special dictionary
	 % then must the IS in ProtoType match this interface and this only.
	[parenthesis_node_open],
	{create_parse_structure(P1)},
	vrmlScene(P1,ProtoType_parsed),
	[parenthesis_node_close],
	{get_parsed(InterfaceDecl_parsed,InterfaceDecl),
	 get_parsed(ProtoType_parsed,ProtoType),
	 lookup_set_prototype(In,NodeTypeId,InterfaceDecl,ProtoType),
	 write('the prototype are set'),nl,
	 write(In),nl,
	 insert_parsed(['PROTO'(NodeTypeId,InterfaceDecl,ProtoType)],In,Out)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
interfaceDeclarations(In, Out) -->
	interfaceDeclaration(In, Out).

interfaceDeclarations(In, Out) -->
	interfaceDeclaration(In, Out0),
	interfaceDeclarations(Out0, Out).

%%%%%%%%%%%%%%%%
interfaceDeclaration(In, Out) -->
	restrictedInterfaceDeclaration(In, Out).

interfaceDeclaration(In, Out) -->
	[id(exposedField)],
	fieldTypeId(FieldType),
	fieldId(FieldId),
	fieldValue(FieldType,FieldValue),
	comment(In,In0),
	{insert_parsed([exposedField(FieldType,FieldId,FieldValue)],In0,Out)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
restrictedInterfaceDeclaration(In,Out) -->
	[id(eventIn)],
	fieldTypeId(FieldType),
	eventInId(EventInId),
	{insert_parsed([eventIn(FieldType,EventInId)],In,Out)}.

restrictedInterfaceDeclaration(In,Out) -->
	[id(eventOut)],
	fieldTypeId(FieldType),
	eventOutId(EventOutId),
	{insert_parsed([eventOut(FieldType,EventOutId)],In,Out)}.

restrictedInterfaceDeclaration(In,Out) -->
	[id(field)],
	fieldTypeId(FieldType),
	fieldId(FieldId),
	fieldValue(FieldType,FieldValue),
	comment(In,In0),
	{insert_parsed([field(FieldType,FieldId,FieldValue)],In0,Out)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
externproto(In,Out) -->
	[id('EXTERNPROTO')],
	 nodeTypeId(NodeTypeId),
	[parenthesis_list_open],
	{create_parse_structure(P)},
	externInterfaceDeclarations(P,ExternDecl_parsed),
	[parenthesis_list_close],
	mfstringValue(MFString),
	{get_parsed(ExternDecl_parsed,ExternDecl),
        lookup_set_extern_prototype(In,NodeTypeId,ExternDecl,MFString),
        insert_parsed(['EXTERNPROTO'(NodeTypeId,ExternDecl,MFString)],In,Out)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
externInterfaceDeclarations(In,Out) -->
	externInterfaceDeclaration(In,Out).

externInterfaceDeclarations(In,Out) -->
	externInterfaceDeclaration(In,Out0),
	externInterfaceDeclarations(Out0,Out).

%%%%%%%%%%%%%%%%
externInterfaceDeclaration(In,Out) -->
	[id(eventIn)],
	fieldTypeId(FieldType),
	eventInId(EventInId),
	{insert_parsed([eventIn(FieldType,EventInId)],In,Out)}.

externInterfaceDeclaration(In,Out) -->
	[id(eventOut)],
	fieldTypeId(FieldType),
	eventOutId(EventOutId),
	{insert_parsed([eventOut(FieldType,EventOutId)],In,Out)}.

externInterfaceDeclaration(In,Out) -->
	[id(field)],
	fieldTypeId(FieldType),
	fieldId(FieldId),
	{insert_parsed([field(FieldType,FieldId)],In,Out)}.

externInterfaceDeclaration(In,Out) -->
	[id(exposedField)],
	fieldTypeId(FieldType),
	fieldId(FieldId),
	{insert_parsed([exposedField(FieldType,FieldId)],In,Out)}.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
routeDeclaration(In,Out) -->
	[id('ROUTE')],
	nodeNameId(NodeNameIdFrom),
	[symbol('.')],
	eventOutId(EventOutId),
	[id('TO')],
	nodeNameId(NodeNameIdTo),
	[id('.')],
	eventInId(EventInId),
	{insert_parsed(['ROUTE'(NodeNameIdFrom,EventOutId,'TO',NodeNameIdTo,EventInId)],In, Out)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
node(In,Out) -->
	comment(In,In0),
	nodeTypeId(NodeTypeId),
	comment(In0,In1),
	[parenthesis_node_open],
	{create_parse_structure(P)},
	nodeGuts(NodeTypeId,P,Guts),
	[parenthesis_node_close],
	{reverse_parsed(Guts,Guts0),
	 create_node(NodeTypeId,Guts0,Node),
	 insert_parsed([Node],In1,Out)}.

node(In,Out) -->
	[id('Script')],
	[parenthesis_node_open],
	{create_parse_structure(P)},
	scriptGuts(P,ScriptGuts),
	[parenthesis_node_close],
	{reverse_parsed(ScriptGuts,ScriptGuts0),
	 create_node('Script',ScriptGuts0,Node),
	 insert_parsed([Node],In,Out)}.

%%The first half of a corrupt parsing.
%node(In,Out) -->
%	comment(In,In0),
%	nodeTypeId(NodeTypeId),
%	comment(In0,In1),
%	look_ahead(Look),
%	{error_vrml(parenthesis_node_open(NodeTypeId,Look))},
%	node_cont(NodeTypeId,In1,Out).

%%The first half of a parted node, correct parsing.
%node(In,Out) -->
%	comment(In,In0),
%	nodeTypeId(NodeTypeId),
%	comment(In0,In1),
%	[parenthesis_node_open],
%	node_cont(NodeTypeId,In1,Out).

%%The second half of the parted node, correct parsing.
%node_cont(NodeTypeId,In,Out) -->
%	{create_parse_structure(P)},
%	nodeGuts(NodeTypeId,P,Guts),
%	[parenthesis_node_close],
%	{reverse_parsed(Guts,Guts0),
%	 create_node(NodeTypeId,Guts0,Node),
%	 insert_parsed([Node],In,Out)}.

%%The second half of the parted node, corrupt parsing.
%node_cont(NodeTypeId,In,Out) -->
%	{create_parse_structure(P)},
%	nodeGuts(NodeTypeId,P,Guts),
%	look_ahead(Look),
%	{error_vrml(parenthesis_node_open(NodeTypeId,Look)),
%	 reverse_parsed(Guts,Guts0),
%	 create_node(NodeTypeId,Guts0,Node),
%	 insert_parsed([Node],In,Out)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nodeGuts(_NodeTypeId,In,Out) -->
	[],
	 {stop_parse(In,Out)}.

nodeGuts(NodeTypeId,In,Out) -->
	nodeGut(NodeTypeId,In,Out0),
	nodeGuts(NodeTypeId,Out0,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
scriptGuts(In, Out) -->
	[],
	{stop_parse(In,Out)}.

scriptGuts(In,Out) -->
	scriptGut(In,Out0),
	scriptGuts(Out0,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nodeGut(NodeTypeId,In,Out) -->
	comment(In,In0),
	fieldId(FieldId),
	{lookup_get_fieldType(In, NodeTypeId, FieldId, FieldType)},
	fieldValue(FieldType,FieldValue),
	comment(In0,In1),
	{create_field(FieldId,FieldValue,Field),
%	!,
	insert_parsed([Field],In1,Out)}.

nodeGut(_NodeTypeId,In,Out) -->
	fieldId(FieldId0),
	[id('IS')],
	fieldId(FieldId1),
	% are we in the prototype
        {%lookup_field(NodeTypeId, FieldId0, FieldId1),
	insert_parsed([FieldId0,'IS',FieldId1],In,Out)}.

nodeGut(_NodeTypeId,In,Out) -->
	eventInId(EventInId0),
        [id('IS')],
	eventInId(EventInId1),
	% are we in the prototype
	{%lookup_eventIn(NodeTypeId, EventInId0, EventInId1),
	insert_parsed([EventInId0,'IS',EventInId1],In,Out)}.

nodeGut(_NodeTypeId,In,Out) -->
	eventOutId(EventOutId0),
	[id('IS')],
	eventOutId(EventOutId1),
	% are we in the prototype
	{%lookup_eventOut(NodeTypeId, EventOutId0, EventOutId1),
	insert_parsed([EventOutId0,'IS',EventOutId1],In,Out)}.

nodeGut(_NodeTypeId,In,Out) -->
	routeDeclaration(In,Out).

nodeGut(_NodeTypeId,In,Out) -->
	protoDeclaration(In,Out).

nodeGut(_,In,Out) -->
	[],
	{stop_parse(In,Out)}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
scriptGut(In,Out) -->
	[id('eventIn')],
	fieldTypeId(FieldType),
	eventInId(EventInId0),
	[id('IS')],
	% are we in the prototype
	eventInId(EventInId1),
	{insert_parsed([FieldType,EventInId0,'IS',EventInId1],In,Out)}.

scriptGut(In,Out) -->
	[id('eventOut')],
	fieldTypeId(FieldType),
	eventOutId(EventOutId0),
	[id('IS')],
	 % are we in the prototype
	eventOutId(EventOutId1),
	{insert_parsed([FieldType,EventOutId0,'IS',EventOutId1],In,Out)}.

scriptGut(In,Out) -->
	[id('field')],
	fieldTypeId(FieldType),
	fieldId(FieldId0),
	[id('IS')],
	 % are we in the prototype
	fieldId(FieldId1),
	{insert_parsed([FieldType,FieldId0,'IS',FieldId1],In,Out)}.

%OBS
scriptGut(In,Out) -->
	nodeGut('Script',In,Out).


scriptGut(In,Out) -->
	restrictedInterfaceDeclaration(In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nodeNameId(Id) -->
	id(Id).

nodeTypeId(Id) -->
	id(Id).

fieldTypeId(Id) -->
	id(Id).

fieldId(Id) -->
	id(Id).

eventInId(Id) -->
	id(Id).

eventOutId(Id) -->
	id(Id).

id(Id) -->
	[id(Id)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
comment(In,Out) -->
	[comment(Com)],
	{insert_parsed([Com],In,Out)}.

comment(In,In) -->
	[].

comment(In,Out) -->
	[comment(Com)],
	{insert_parsed([Com],In,In0)},
	comment(In0,Out).



re(Stop_sign) -->
	[Stop_sign].
	
re(Stop_sign) -->
	[Skipped],
	{write(Skipped),nl},
	re(Stop_sign).
