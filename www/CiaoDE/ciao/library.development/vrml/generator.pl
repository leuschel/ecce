:- module(generator, [generator/2,nodeDeclaration/4], 
	             [assertions,basicmodes,dcg,regtypes]).

:- include(library(iso)).
:- use_module(library(iso_byte_char)).
:- use_module(library(basicprops)).
%%:- use_module(library(lists),[append/3]).
:- use_module(lookup).
:- use_module(io,[convert_atoms_to_string/2]).
:- use_module(generator_util,[reading/4,
	                      reading/5,
			      reading/6,
			      open_node/6,
			      close_node/5,
			      close_nodeGut/4,
			      open_PROTO/4,
			      close_PROTO/6,
			      open_EXTERNPROTO/5,
			      close_EXTERNPROTO/6,
			      open_DEF/5,
			      close_DEF/5,
			      open_Script/5,
			      close_Script/5,
			      start_vrmlScene/4]).
:- use_module(parser_util,[create_parse_structure/1,
	                   create_parse_structure/2,
			   create_parse_structure/3,
			   push_whitespace/3,
			   get_indentation/2]).
:- use_module(error).

%%%:- use_module(internal_types).
%%%:- use_module(i_o).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred generator(+Terms,-VRML)
	:: list(terms) * string
        # "This predicate is the generator of VRML code. It accepts a
           list of terms that is correct VRML code, other kind of terms 
           will be rejected will errormessage accordingly. The output
           is a string of correct VRML code, acceptable for VRML browsers.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generator(Terms,VRML) :-
	create_parse_structure(Terms,Structure),
	catch( vrmlScene_first(Structure,_Out,VRML_atoms,[]), 
	Msg, output_error(Msg)),
	convert_atoms_to_string(VRML_atoms,VRML),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
vrmlScene_first(In,Out) -->
	header(In,Out0),
	vrmlScene(Out0,Out).
	
%We read comment('#VRML V2.0 utf8') and the following can be skipped.
header(In,Out) -->
	reading(header,In,Out).

header(In,Out) -->
	reading(error_header,In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
vrmlScene(In,Out) -->
	declarations(In,Out).

vrmlScene(In,Out) -->
	reading(error_declaration,In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
declarations(In,Out) -->
	reading(empty,In,Out).

declarations(In,Out) -->
	declaration(In,Out0),
	declarations(Out0,Out).

%%%%%%%%%%%%%%%%
declaration(In,Out) -->
	reading('NULL',In,Out).

declaration(In,Out) -->
	reading(comment,In,In0),
	declaration(In0,Out).

declaration(In,Out) -->
	protoDeclaration(In,Out).

declaration(In,Out) -->
	routeDeclaration(In,Out).

declaration(In,Out) -->
	reading(comment,In,Out).

declaration(In, Out) -->
	nodeDeclaration(In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nodeDeclaration(In,Out) -->
	reading('DEF',In),
	open_DEF(In,In0,NodeIn),
	node(NodeIn,NodeOut),
	close_DEF(NodeOut,In0,Out).

nodeDeclaration(In,Out) -->
	reading('USE',In,Out).

nodeDeclaration(In,Out) -->
	node(In,Out).

nodeDeclaration(In,Out) -->
	reading(error_nodeDeclaration,In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
protoDeclaration(In,Out) -->
	proto(In,Out).

protoDeclaration(In,Out) -->
	externproto(In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
proto(In,Out) -->
	reading('PROTO',In),
	open_PROTO(In,DeclIn),
	interfaceDeclarations(DeclIn,DeclOut),
	start_vrmlScene(In,SceneIn),
	vrmlScene(SceneIn,SceneOut),
	close_PROTO(DeclOut,SceneOut,In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
interfaceDeclarations(In,Out) -->
	reading(empty,In,Out).

interfaceDeclarations(In,Out) -->
	interfaceDeclaration(In,In0),
	interfaceDeclarations(In0,Out).

%%%%%%%%%%%%%%%%
interfaceDeclaration(In, Out) -->
	reading(comment,In,In0),
	interfaceDeclaration(In0,Out).

interfaceDeclaration(In,Out) -->
	restrictedInterfaceDeclaration(In,Out).

interfaceDeclaration(In,Out) -->
	reading(exposedField,In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
restrictedInterfaceDeclaration(In,Out) -->
	reading(restrictedInterfaceDeclaration,In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
externproto(In,Out) -->
	reading('EXTERNPROTO',In),
	open_EXTERNPROTO(In,DeclIn,StringIn),
	externInterfaceDeclarations(DeclIn,DeclOut),
	reading(mfstringValue,StringIn,StringOut),
	close_EXTERNPROTO(DeclOut,StringOut,In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
externInterfaceDeclarations(In,Out) -->
	reading(empty,In,Out).

externInterfaceDeclarations(In,Out)-->
	externInterfaceDeclaration(In,In0),
	externInterfaceDeclarations(In0,Out).

%%%%%%%%%%%%%%%%
externInterfaceDeclaration(In,Out) -->
	reading(comment,In,In0),
	externInterfaceDeclaration(In0,Out).

externInterfaceDeclaration(In,Out) -->
	reading(externInterfaceDeclaration,In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
routeDeclaration(In,Out) -->
	reading('ROUTE',In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
node(In,Out) -->
	reading('Script',In),
	open_Script(In,In0,ScriptGutsIn),
	scriptGuts(ScriptGutsIn,ScriptGutsOut),
	close_Script(ScriptGutsOut,In0,Out).

node(In,Out) -->
	reading(node,In),
	open_node(In,In0,NodeGutsIn,NodeNameId),
	nodeGuts(NodeNameId,NodeGutsIn,NodeGutsOut),
	close_node(NodeGutsOut,In0,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nodeGuts(_Name,In,Out) -->
	close_nodeGut(In,Out).

nodeGuts(Name,In,Out) -->
	nodeGut(Name,In,Out0),
	nodeGuts(Name,Out0,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
scriptGuts(In,Out) -->
	reading(empty,In,Out).

scriptGuts(In,Out) -->
	scriptGut(In,Out0),
	scriptGuts(Out0,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nodeGut(Name,In,Out) -->
	reading(comment,In,In0),
	nodeGut(Name,In0,Out).

nodeGut(Name,In,Out) -->
	reading('IS',Name,In,Out).

nodeGut(Name,In,Out) -->
	reading(nodeGut,Name,In,Out).

nodeGut(_Name,In,Out) -->
	routeDeclaration(In,Out).

nodeGut(_Name,In,Out) -->
	protoDeclaration(In,Out).

nodeGut(Name,In,Out) -->
	reading(error_nodeGut(Name),In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
scriptGut(In,Out) -->
	reading(comment,In,In0),
	scriptGut(In0,Out).

scriptGut(In,Out) -->
	restrictedInterfaceDeclaration(In,Out).

scriptGut(In,Out) -->
	nodeGut('Script',In,Out).
