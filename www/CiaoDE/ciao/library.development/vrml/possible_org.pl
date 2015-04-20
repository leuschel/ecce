:- module(possible,[continue/3,look_ahead/3],[dcg]).

:- use_module(library(lists)).

continue(Clause) -->
	look_ahead(Ahead),
	{possible(Clause,Possibilities),
	 memberchk(Ahead,Possibilities)}.

look_ahead(Token,[Val|Rest],[Val|Rest]) :-
	Val =.. [Token|_Value].

memberchk(_Element,[]) :-
	!,
	fail.

memberchk(Element,[Element|_More]) :-
	!.

memberchk(Element,[_Something|More]) :-
	memberchk(Element,More).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

possible(vrmlScene,[comment,id,'Script','DEF','USE','PROTO','EXTERNPROTO',
	            'ROUTE','NULL']).

possible(declarations,[comment,id,'Script','DEF','USE','PROTO','EXTERNPROTO',
	            'ROUTE','NULL']).

possible(declaration,[comment,id,'Script','DEF','USE','PROTO','EXTERNPROTO',
	            'ROUTE','NULL']).

possible(nodeDeclaration,[comment,id,'Script','DEF','USE']).

possible(protoDeclaration,[comment,'PROTO','EXTERNPROTO']).

possible(proto,[comment,'PROTO','EXTERNPROTO']).

possible(interfaceDeclarations, [comment,eventIn,eventOut,field,exposedField]).

possible(interfaceDeclaration, [comment,eventIn,eventOut,field,exposedField]).

possible(restrictedInterfaceDeclaration,[comment,eventIn,eventOut,field]).

possible(externproto,[comment,'EXTERNPROTO']).

possible(externInterfaceDeclarations,[comment,eventIn,eventOut,field,
	                              exposedField]).

possible(externInterfaceDeclaration,[comment,eventIn,eventOut,field,
	                              exposedField]).

possible(routeDeclaration,[comment,'ROUTE']).

possible(node,[comment,id,'Script']).

possible(nodeGuts,[comment,id,'ROUTE','PROTO','EXTERNPROTO']).

possible(nodeGut,[comment,id,'ROUTE','PROTO','EXTERNPROTO']).

possible(scriptGuts,[comment,id,'ROUTE','PROTO','EXTERNPROTO',
	             eventIn,eventOut,field]).

possible(scriptGut,[comment,id,'ROUTE','PROTO','EXTERNPROTO',
	             eventIn,eventOut,field]).

possible(fieldType,[comment,
	            'MFColor','MFFloat','MFInt32','MFNode','MFRotation',
	            'MFString','MFVec2f','MFVec3f','SFBool','SFColor',
		    'SFFloat','SFImage','SFInt32','SFNode','SFRotation',
		    'SFString','SFTime','SFVec2f','SFVec3f']).

possible(fieldValue,[comment,'TRUE','FALSE',float,integer,hex,exp,id,'Script',
	             'DEF','USE','NULL',string,parenthesis_list_open]).
