:- module(possible,[continue/3],[dcg]).

:- use_module(library(lists)).

continue(Clause) -->
	
	{possible(Clause,Possibilities,Type)},
	 look_ahead(Type,Ahead),
	 {memberchk(Ahead,Possibilities)}.

look_ahead(token,Token,[Val|Rest],[Val|Rest]) :-
	Val =.. [Token|_Value].

look_ahead(word,Word,[Val|Rest],[Val|Rest]) :-
	Val =.. [_Token,Word|_Value].

memberchk(_Element,[]) :-
	!,
	fail.

memberchk(Element,[Element|_More]) :-
	!.

memberchk(Element,[_Something|More]) :-
	memberchk(Element,More).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

possible(vrmlScene,[comment,whitespace,id],token).

possible(declarations,[comment,whitespace,id],token).

possible(declaration,[comment,whitespace,id],token).

possible(nodeDeclaration,[comment,whitespace,id],token).

possible(protoDeclaration,['PROTO','EXTERNPROTO'],word).

possible(proto,['PROTO','EXTERNPROTO'],word).

possible(interfaceDeclarations, [eventIn,eventOut,field,exposedField],word).

possible(interfaceDeclaration, [eventIn,eventOut,field,exposedField],word).

possible(restrictedInterfaceDeclaration,[eventIn,eventOut,field],word).

possible(externproto,['EXTERNPROTO'],word).

possible(externInterfaceDeclarations,[eventIn,eventOut,field,exposedField],word).

possible(externInterfaceDeclaration,[eventIn,eventOut,field,exposedField],word).

possible(routeDeclaration,['ROUTE'],word).

possible(node,[comment,whitespace,id],token).

possible(nodeGuts,[comment,whitespace,id],token).

possible(nodeGut,[comment,whitespace,id],token).

possible(scriptGuts,[comment,whitespace,id],token).

possible(scriptGut,[comment,whitespace,id],token).

possible(fieldType,['MFColor','MFFloat','MFInt32','MFNode','MFRotation',
	            'MFString','MFVec2f','MFVec3f','SFBool','SFColor',
		    'SFFloat','SFImage','SFInt32','SFNode','SFRotation',
		    'SFString','SFTime','SFVec2f','SFVec3f'],word).

possible(fieldValue,[comment,whitespace,id,float,integer,hex,exp,id,string,
	             parenthesis_list_open],token).
