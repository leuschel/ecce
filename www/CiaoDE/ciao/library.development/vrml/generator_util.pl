:- module(generator_util,
	[   reading/4,
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
	    decompose_field/3,
	    indentation_list/2,
	    start_vrmlScene/4,
	    remove_comments/4
	],[iso,dcg]).

:- use_module(engine(basic_props)).
:- use_module(error,[error_vrml/1]).
:- use_module(library(lists),[append/3]).
:- use_module(io,[out/3]).
:- use_module(field_value_check,[mfstringValue/7]).
:- use_module(lookup,[lookup_get_fieldType/4,
	              lookup_check_node/4,
		      lookup_check_field/6,
		      lookup_field/4,
		      lookup_route/5,
		      lookup_field_access/4,
		      lookup_check_interface_fieldValue/8,
		      lookup_set_prototype/4,
		      lookup_set_def/3]).
:- use_module(parser_util,[look_first_parsed/2,
	                   get_first_parsed/3,
			   get_indentation/2,
			   get_parsed/2,
			   create_parse_structure/3,
			   push_dictionaries/3,
			   inc_indentation/2,
			   dec_indentation/2,
			   add_indentation/3,
			   reduce_indentation/3,
			   inside_proto/1,
			   strip_exposed/2,
			   strip_restricted/2,
			   strip_clean/2,
			   set_environment/3,
			   create_environment/4
			   ]).

:- set_prolog_flag(multi_arity_warnings, off).
:- discontiguous([reading/4,reading/5,reading/6]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reading(empty,In,In) -->
	{ get_parsed(In,Parsed),
	  Parsed == []
	}.

reading(header,In,Out) -->
	{ get_first_parsed(In,Out,Comment),
	  name(Comment,List),
	  List = [35,86,82,77,76,32,86,50,46,48,32,117,116,102,56|_More]
	},
	out([Comment,'\n\n']).

reading('NULL',In,Out) -->
	{ get_first_parsed(In,Out,First),
	  'NULL' == First
	},
	out(['NULL\n']).

reading(comment,In,Out) -->
	{ get_first_parsed(In,Out,Comment),
	  atomic(Comment),
	  name(Comment,List),
	  List = [35|_More],
	  indentation_list(In,Indent)
	},
	out(Indent),
	out([Comment]).


reading('DEF',In) -->
	{ look_first_parsed(In,First),
	  get_name(First,Name),
	  'DEF' == Name
	}.

reading('USE',In,Out) -->
	{ get_first_parsed(In,Out,First),
	  decompose_field(First,Name,[NodeName]),
	  'USE' == Name,
	  indentation_list(In,Indent)
	},
	out(Indent),
	out(['USE ',NodeName]).
	%checkup the NodeName and write when done
	

reading('IS',NodeTypeId,In,Out) -->
	{ inside_proto(In),
	  get_first_parsed(In,In0,First0),
	  get_first_parsed(In0,In1,First1),
	  First1 == 'IS',
	  get_first_parsed(In1,Out,First2),
	  strip_clean(First0,Clean0),
	  strip_clean(First2,Clean2),
	  lookup_field_access(In,NodeTypeId,Clean2,Clean0),
	  indentation_list(In,Indent)
	},
	out(Indent),
	out([First0,' IS ',First2,'\n']).

reading(node,In) -->
	{ look_first_parsed(In, Node),
	  get_name(Node,Name)
	},
	lookup_check_node(In,Name).

reading('Script',In) -->
	{ look_first_parsed(In, First),
	  get_name(First,Name),
	  'Script' == Name
	}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reading(nodeGut,Name,In,Out) -->
	{ get_first_parsed(In,In0,Field)
	},
	lookup_check_field(In0,Out,Name,Field),
	out(['\n']).	  

%%%%%%%%%%%%%%%%
reading('PROTO',In) -->
	{ look_first_parsed(In,First),
	  get_name(First, Name),
	  'PROTO' == Name
	}.

reading('EXTERNPROTO',In) -->
	{ look_first_parsed(In,First),
	  get_name(First, Name),
	  'EXTERNPROTO' == Name
	}.

reading(mfstringValue,In,Out) -->
	{ get_parsed(In,MFStringValue),
	  indentation_list(In,Indent)
	},
	out(Indent),
	out([']\n']),
	mfstringValue(MFStringValue,In,Out,_,[]).
	

reading(exposedField,In,Out) -->
	{ get_first_parsed(In,In0,First),
	  get_name(First, Name),
	  exposedField == Name,
	  decompose_term(First,[Acc,Type,Id,Value])
	},
	lookup_check_interface_fieldValue(In0,Out,Acc,Type,Id,Value).

% There are different types of field and exposedField but that will
% be handled in writing.
reading(restrictedInterfaceDeclaration,In,Out) -->
	{ get_first_parsed(In,In0,First),
	  get_name(First, Name),
	  ( field == Name
	  ; eventIn == Name
	  ; eventOut == Name
	  )
	},
	( { Name == field }
	->{ decompose_term(First,[Acc,Type,Id,Value]) },
          lookup_check_interface_fieldValue(In0,Out,Acc,Type,Id,Value)
	; { decompose_term(First,[Acc,Type,Id]),
	    indentation_list(In0,Indent),
	    In0 = Out
	  },
	  out(Indent),
	  out([Acc,' ',Type,' ',Id,'\n'])
	).

reading(externInterfaceDeclaration,In,Out) -->
	{ get_first_parsed(In,Out,First),
	  decompose_term(First,[Acc,Type,Id]),
	  ( field == Acc
	  ; eventIn == Acc
	  ; eventOut == Acc
	  ; exposedField == Acc
	  ),
	  indentation_list(In,Indent)
	},
	out(Indent),
	out([Acc,' ',Type,' ',Id,'\n']).

reading('ROUTE',In,Out) -->
	{ get_first_parsed(In,Out,First),
	  decompose_term(First,[Name,NodeOut0,FieldOut0,'TO',NodeIn0,FieldIn0]),
	  'ROUTE' == Name,
	  strip_clean(NodeOut0,NodeOut),
	  strip_clean(FieldOut0,FieldOut),
	  strip_clean(NodeIn0,NodeIn),
	  strip_clean(FieldIn0,FieldIn),
	  lookup_route(In,NodeOut,FieldOut,NodeIn,FieldIn)
	},
	out(['ROUTE ',NodeOut0,'.',FieldOut0,' TO ',NodeIn0,'.',FieldIn0,'\n']).


reading(error_nodeGut(Name),In,Out) -->
	{ get_first_parsed(In,Out,First),
	  error_vrml(nodeGuts(Name,First))
	}.

reading(error_nodeDeclaration,In,Out) -->
	{ get_first_parsed(In,Out,First),
	  error_vrml(nodeDeclaration(First))
	}.

reading(error_header,In,Out) -->
	{ get_first_parsed(In,Out,First),
	  error_vrml(header(First))
	}.

reading(error_declaration,In,Out) -->
	{ get_first_parsed(In,Out,First),
	  error_vrml(declarations(First))
	}.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
indentation_list(In,List) :-
	get_indentation(In,Indent),
	indentation_list_num(Indent,List).

indentation_list_num(0,[]).
indentation_list_num(Nr,['   '|More]) :-
	Nr > 0,
	New is Nr - 1,
	indentation_list_num(New, More).


count_elements([],0).
count_elements([_|R],Inc) :-
	count_elements(R,New),
	Inc is New + 1.

names_to_steps([],0).
names_to_steps([Name|Rest],Ind) :-
	name(Name,List),
	count_elements(List,Count),
	names_to_steps(Rest,New),
	Ind is Count + New.
	
names_to_indentation_steps(Names,Steps) :-
	names_to_steps(Names,Letters),
	Steps is Letters // 4.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_nodeGuts(Node,Guts) -->
	{ decompose_term(Node,[_Name|Guts]) }.

get_scriptGuts(Node,Guts) -->
	{ decompose_term(Node,[_Name|Guts]) }.

get_fieldValue(Field,Value) :-
	decompose_field(Field,_Name,Value).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
open_node(In,Out,NodeGutsStruct,NodeNameId) -->
	{ get_first_parsed(In,In0,First),
	  decompose_field(First,NodeNameId,[NodeGuts]),
	  inc_indentation(In0,Out),
	  create_parse_structure(NodeGuts,Out,NodeGutsStruct)
	}.

close_node(NodeStruct,In,Out) -->
	{ push_dictionaries(NodeStruct,In,Out0),
	  dec_indentation(Out0,Out)
	}.

close_nodeGut(In,Out) -->
	{ get_parsed(In,Parsed),
	  Parsed == [],
	  dec_indentation(In,In0),
	  indentation_list(In0,Indent)
	},
	out(Indent),
	out(['}\n']),
	{ inc_indentation(In0,Out)
	}.
	
open_PROTO(In,DeclIn) -->
	{ look_first_parsed(In,First),
	  decompose_term(First,['PROTO',NodeTypeId,Interface,_Scene]),
	  inc_indentation(In,In0),
	  create_environment(In0,'PROTO',NodeTypeId,Env),
	  create_parse_structure(Interface,In0,DeclIn0),
	  set_environment(Env,DeclIn0,DeclIn),	
	  indentation_list(In,Indent)
	},
	out(Indent),
	out(['PROTO ', NodeTypeId,' [\n']).

start_vrmlScene(In,SceneIn) -->
	{
	  look_first_parsed(In,First),
	  decompose_term(First,['PROTO',NodeTypeId,Interface,Scene]),
	  inc_indentation(In,In0),
	  create_environment(In0,'PROTO',NodeTypeId,Env),
	  create_parse_structure(Scene,In0,SceneIn0),
	  set_environment(Env,SceneIn0,SceneIn),
	  lookup_set_prototype(In,NodeTypeId,Interface,Scene),
	  indentation_list(In0,Indent)
	},
	out(Indent),
	out(['] {\n']).
	  
close_PROTO(_DeclIn,SceneIn,In,Out) -->
	{ 
	  get_first_parsed(In,In0,_First),
	  push_dictionaries(SceneIn,In0,In1),
	  dec_indentation(In1,Out),
	  indentation_list(Out,Indent)
	},
	out(Indent),
	out(['\n\n']).
	
open_EXTERNPROTO(In,DeclIn,StringIn) -->
	{ look_first_parsed(In,First),
	  decompose_term(First,['EXTERNPROTO',NodeTypeId,Interface,String]),
	  inc_indentation(In,In0),
	  create_environment(In0,'EXTERNPROTO',NodeTypeId,Env),
	  create_parse_structure(Interface,In0,DeclIn0),
	  create_parse_structure(String,In0,StringIn0),
	  set_environment(Env,DeclIn0,DeclIn),	
	  set_environment(Env,StringIn0,StringIn),
	  indentation_list(In,Indent)
	},
	out(Indent),
	out(['EXTERNPROTO ', NodeTypeId,' [\n']).	  

close_EXTERNPROTO(_DeclIn,_StringIn,In,Out) -->
	{ 
	  get_first_parsed(In,In0,First),
	  decompose_term(First,['EXTERNPROTO',NodeTypeId,Interface,String]),
	  lookup_set_prototype(In,NodeTypeId,Interface,String),
	  dec_indentation(In0,Out)
	}.

open_DEF(In,Out,NodeIn) -->
	{ 
	  get_first_parsed(In,In0,First),
	  decompose_term(First,['DEF',Name,Node]),
	  lookup_set_def(In0,Name,Node),
	  create_parse_structure(Node,In0,NodeIn0),
	  create_environment(In0,'DEF',Name,Env),
	  set_environment(Env,NodeIn0,NodeIn),
	  indentation_list(In0,Indent),
	  inc_indentation(In0,Out)
	},
	out(Indent),
	out(['DEF ',Name,'\n']).
	    
close_DEF(NodeIn,In,Out) -->
	{ push_dictionaries(NodeIn,In,Out)
	}.


open_Script(In,Out,ScriptIn) -->
	{  get_first_parsed(In,In0,First),
	  decompose_field(First,_Script,[ScriptGuts]),
	  inc_indentation(In0,Out),
	  create_parse_structure(ScriptGuts,Out,ScriptIn)
	}.

close_Script(ScriptStruct,In,Out) -->
	{ push_dictionaries(ScriptStruct,In,Out0),
	  dec_indentation(Out0,Out)
	}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_comments([],[],[],[]).
remove_comments(List,Before,Value,After) :-
	list(List),
	get_comments(List,Before,[Value|After]).

get_comments([],[],[]).
get_comments([C|Rest],[C|Before],After):-
	atomic(C),
	name(C,[35|_Rest]),
	get_comments(Rest,Before,After).

get_comments([Value|After],[],[Value|After]).

	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	    
	  


%%%%%%%%%%%%%%%%
decompose_field(Field,Name,Guts) :-
	Field =..[Name|Guts].

get_name(Term,Name) -->
	{ get_name(Term,Name) }.

get_name(Term,Name) :-
	decompose_term(Term,[Name|_Rest]).

decompose_term(Term,List) :-
	Term =.. List.
