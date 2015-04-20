:- module(field_value,[fieldValue/6,mfstringValue/5], [dcg]).


:- include(library(assertions)).
%:- include(library(basicmodes)).
%:- include(library(types)).

%:- include(library(iso)).
%:- use_module(library(iso_byte_char)).
%:- use_module(library(basicprops)).
:- use_module(library(lists)).
:- use_module(parser,[nodeDeclaration/4]).
:- use_module(parser_util).
:- use_module(error).

:- set_prolog_flag(multi_arity_warnings, off).
%:- discontiguous([token_read/3]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-pred fieldValue(+ParseIn,-ParseOut,+FieldTypeId,-FieldValue)
 :: parse * parse * atm * list(term)
  ; "The predicate read the fieldValue from the input token stream
     and return the value of the parsing. The resulting list might
     be of numbers, strings or VRML code dependnig on the FieldTypeId.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fieldValue(In,Out,'MFColor',Value) -->
	mfcolorValue(In,Out,Value).

fieldValue(In,Out,'MFFloat',Value) -->
	mffloatValue(In,Out,Value).

fieldValue(In,Out,'MFInt32',Value) -->
	mfint32Value(In,Out,Value).

fieldValue(In,Out,'MFNode',Value) -->
	mfnodeValue(In,Out,Value).

fieldValue(In,Out,'MFRotation',Value) -->
	mfrotationValue(In,Out,Value).

fieldValue(In,Out,'MFString',Value) -->
	mfstringValue(In,Out,Value).

fieldValue(In,Out,'MFVec2f',Value) -->
	mfvec2fValue(In,Out,Value).

fieldValue(In,Out,'MFVec3f',Value) -->
	mfvec3fValue(In,Out,Value).

fieldValue(In,Out,'SFBool',Value) -->
	sfboolValue(In,Out,Value).

fieldValue(In,Out,'SFColor',Value) -->
	sfcolorValue(In,Out,Value).

fieldValue(In,Out,'SFFloat',Value) -->
	sffloatValue(In,Out,Value).

fieldValue(In,Out,'SFImage',Value) -->
	sfimageValue(In,Out,Value).

fieldValue(In,Out,'SFInt32',Value) -->
	sfint32Value(In,Out,Value).

fieldValue(In,Out,'SFNode',Value) -->
	sfnodeValue(In,Out,Value).

fieldValue(In,Out,'SFRotation',Value) -->
	sfrotationValue(In,Out,Value).

fieldValue(In,Out,'SFString',Value) -->
	sfstringValue(In,Out,Value).

fieldValue(In,Out,'SFTime',Value) -->
	sftimeValue(In,Out,Value).

fieldValue(In,Out,'SFVec2f',Value) -->
	sfvec2fValue(In,Out,Value).

fieldValue(In,Out,'SFVec3f',Value) -->
	sfvec3fValue(In,Out,Value).

fieldValue(_,_,Type,_) -->
	{ error_vrml(fieldValue(Type)) }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mfcolorValue(In,Out,Value) -->
	sfcolorValue(In,Out,Value).

mfcolorValue(In,Out,[SFColorValues]) -->
	[parenthesis_list_open],
	sfcolorValues(In,Out,SFColorValues),
	[parenthesis_list_close].


mfcolorValue(In,Out,[Comment]) -->
	[parenthesis_list_open],
	fillout(In,Out,Comment),
	[parenthesis_list_close].

sfcolorValues(In,Out,Value) -->
	sfcolorValue(In,Out,Value).

sfcolorValues(In,Out,Values) -->
	sfcolorValue(In,In0,Value),
	sfcolorValues(In0,Out,Rest),
	{append(Value,Rest,Values)}.

sfcolorValue(In,Out,Value) -->
	read_float(In,In0,C0),
	read_float(In0,In1,C1),
	read_float(In1,Out,C2),
	{append(C0,C1,C01),
	 append(C01,C2,Value)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mffloatValue(In,Out,Value) -->
	sffloatValue(In,Out,Value).

mffloatValue(In,Out,[Value]) -->
	[parenthesis_list_open],
	 fillout(In,Out,Value),
	[parenthesis_list_close].

mffloatValue(In,Out,[Values]) -->
	[parenthesis_list_open],
	sffloatValues(In,Out,Values),
	[parenthesis_list_close].
	
sffloatValues(In,Out,Value) -->
	sffloatValue(In,Out,Value).

sffloatValues(In,Out,Values) -->
	sffloatValue(In,In0,Value),
	sffloatValues(In0,Out,Rest),
	{append(Value,Rest,Values)}.

sffloatValue(In,Out,Value) -->
	read_float(In,Out,Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mfint32Value(In,Out,Value) -->
	sfint32Value(In,Out,Value).

mfint32Value(In,Out,[Val]) -->
	[parenthesis_list_open],
	 fillout(In,Out,Val),
	[parenthesis_list_close].

mfint32Value(In,Out,[Values]) -->
	[parenthesis_list_open],
	sfint32Values(In,Out,Values),
	[parenthesis_list_close].
	

sfint32Values(In,Out,Value) -->
	sfint32Value(In,Out,Value).

sfint32Values(In,Out,Values) -->
	sfint32Value(In,In0,Value),
	sfint32Values(In0,Out,Rest),
	{append(Value,Rest,Values)}.

sfint32Value(In,Out,Value) -->
	read_integer(In,Out,Value).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mfnodeValue(In,Out,[Values]) -->
	[parenthesis_list_open],
	{create_parse_structure(In,P)},
	nodeDeclarations(P,Values_parsed0),
	{push_whitespace(Values_parsed0,In,Out),
	 reverse_parsed(Values_parsed0,Values_parsed),
	 get_parsed(Values_parsed,Values)},
	[parenthesis_list_close].

mfnodeValue(In,Out,Value) -->
	{create_parse_structure(In,P)},
	nodeDeclaration(P,Value_parsed0),
	{push_whitespace(Value_parsed0,In,Out),
	 reverse_parsed(Value_parsed0,Value_parsed),
	 get_parsed(Value_parsed,Value)}.

mfnodeValue(In,Out,[Value]) -->
	[parenthesis_list_open],
	fillout(In,Out,Value),
	[parenthesis_list_close].

sfnodeValue(In,Out,Value) -->
	[id('NULL')],
	fillout(In,Out,Comment),
	{ ( Comment == []
	  -> Value = ['NULL']
	  ;  Value = [['NULL'|Comment]]
	  )
	}.

sfnodeValue(In,Out,Value) -->
	{create_parse_structure(In,P)},
	nodeDeclaration(P,Value_parsed),
	{push_whitespace(Value_parsed,In,Out),
	 get_parsed(Value_parsed,Value)}.

sfnodeValue(In,Out,Node) -->
	at_least_one(In,In0),
	sfnodeValue(In0,Out,Node).
	
nodeDeclarations(In,Out) -->
	[],
	{stop_parse(In,Out) }.


nodeDeclarations(In,Values) -->
	nodeDeclaration(In,Value),
	nodeDeclarations(Value,Values).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mfrotationValue(In,Out,Value) -->
	sfrotationValue(In,Out,Value).

mfrotationValue(In,Out,[Comment]) -->
	[parenthesis_list_open],
	fillout(In,Out,Comment),
	[parenthesis_list_close].

mfrotationValue(In,Out,[Values]) -->
	[parenthesis_list_open],
	sfrotationValues(In,Out,Values),
	[parenthesis_list_close].
	
sfrotationValues(In,Out,Value) -->
	sfrotationValue(In,Out,Value).

sfrotationValues(In,Out,Values) -->
	sfrotationValue(In,In0,Value),
	sfrotationValues(In0,Out,Rest),
	{append(Value,Rest,Values)}.

sfrotationValue(In,Out,Values) -->
	read_float(In,In0,X),
	read_float(In0,In1,Y),
	read_float(In1,In2,Z),
	read_float(In2,Out,R),
	{append(X,Y,XY),
	 append(XY,Z,XYZ),
	 append(XYZ,R,Values)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred mfstringValue(+ParseIn,-ParseOut,-Value)
 :: parse * parse * list(str)
  ; "The predicate is exported for 'EXTERNPROTO' use, where names for 
     locations are given.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mfstringValue(In,Out,Value) -->
	sfstringValue(In,Out,Value).

mfstringValue(In,Out,[Values]) -->
	[parenthesis_list_open],
	sfstringValues(In,Out,Values),
	[parenthesis_list_close].
	
mfstringValue(In,Out,[Value]) -->
	[parenthesis_list_open],
	fillout(In,Out,Value),
	[parenthesis_list_close].

sfstringValues(In,Out,Value) -->
	sfstringValue(In,Out,Value).

sfstringValues(In,Out,Values) -->
	sfstringValue(In,In0,Value),
	sfstringValues(In0,Out,Rest),
	{append(Value,Rest,Values)}.

sfstringValue(In,In,[Value]) -->
	[string(Value)].

sfstringValue(In,Out,Value) -->
	at_least_one(In,In0,Fill),
	sfstringValue(In0,Out,String),
	{append(Fill,String,Value)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mfvec2fValue(In,Out,Value) -->
	sfvec2fValue(In,Out,Value).

mfvec2fValue(In,Out,[Value]) -->
	[parenthesis_list_open],
	fillout(In,Out,Value),
	[parenthesis_list_close].

mfvec2fValue(In,Out,[Values]) -->
	[parenthesis_list_open],
	sfvec2fValues(In,Out,Values),
	[parenthesis_list_close].
	

sfvec2fValues(In,Out,Value) -->
	sfvec2fValue(In,Out,Value).

sfvec2fValues(In,Out,Values) -->
	sfvec2fValue(In,In0,Value),
	sfvec2fValues(In0,Out,Rest),
	{append(Value,Rest,Values)}.

sfvec2fValue(In,Out,Value) -->
	read_float(In,In0,V0),
	read_float(In0,Out,V1),
	{append(V0,V1,Value)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mfvec3fValue(In,Out,Value) -->
	sfvec3fValue(In,Out,Value).

mfvec3fValue(In,Out,[Value]) -->
	[parenthesis_list_open],
	fillout(In,Out,Value),
	[parenthesis_list_close].

mfvec3fValue(In,Out,[Values]) -->
	[parenthesis_list_open],
	sfvec3fValues(In,Out,Values),
	[parenthesis_list_close].
	

sfvec3fValues(In,Out,Value) -->
	sfvec3fValue(In,Out,Value).

sfvec3fValues(In,Out,Values) -->
	sfvec3fValue(In,In0,Value),
	sfvec3fValues(In0,Out,Rest),
	{append(Value,Rest,Values)}.

sfvec3fValue(In,Out,Value) -->
	read_float(In,In0,V0),
	read_float(In0,In1,V1),
	read_float(In1,Out,V2),
	{append(V0,V1,V01),
	 append(V01,V2,Value)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sfboolValue(In,Out,Value) -->
	read_bool(In,Out,Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sfimageValue(In,Out,Value) -->
	read_image(In,In0,I0),
	read_image(In0,In1,I1),
	read_image(In1,In2,I2),
	{get_number(I0,N0),
	get_number(I1,N1),
	Number is N0 * N1},
	sfimageValues(In2,Out,Number, Values),
	{append(I0,I1,I01),
	 append(I01,I2,I012),
	 append(I012,Values,Value)}.

sfimageValues(In,In,0,[]) -->
	[].

sfimageValues(In,Out,Number,Values) -->
	read_integer(In,In0,Value),
	{Next is Number - 1},
	sfimageValues(In0,Out,Next, Rest),
	{append(Value,Rest,Values)}.

get_number([N|_Rest],N) :-
	number(N).

get_number([List|Rest],Number) :-
	(atomic(List)
	->
	get_number(Rest,Number)
	;
	get_number(List,Number)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sftimeValue(In,Out,Value) -->
	read_float(In,Out,Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_bool(In,Out,Value) -->
	read_bool0(Bool),
	fillout(In,Out,Comment),
	{ (Comment == []
	->
	Value = App
	;
	Value = [App])},
	{append(Bool,Comment,App)}.

read_bool(In,Out,[Value]) -->
	at_least_one(In,In0,Com),
	read_bool(In0,Out,Bool),
	{append(Com,Bool,Value)}.

read_bool0(['TRUE']) -->
	[id('TRUE')].

read_bool0(['FALSE']) -->
	[id('FALSE')].

%%%%%%%%%%%%%%%%
read_float(In,Out,Value) -->
	read_float0(Float),
	fillout(In,Out,Comment),
	{ (Comment == []
	->
	Value = App
	;
	Value = [App])},
	{append(Float,Comment,App)}.

read_float(In,Out,[Value]) -->
	at_least_one(In,In0,Comment),
	read_float(In0,Out,Float),
	{append(Comment,Float,Value)}.

read_float0([Float]) -->
	[float(Float)].

read_float0([Int]) -->
	[integer(Int)].

read_float0([Exp]) -->
	[exp(Exp)].

%%%%%%%%%%%%%%%%
read_integer(In,Out,Value) -->
	read_integer0(Int),
	fillout(In,Out,Comment),
	{(Comment == []
	->
	Value = App
	;
	Value = [App])},
	{append(Int,Comment,App)}.

read_integer(In,Out,[Value]) -->
	at_least_one(In,In0,Com),
	read_integer(In0,Out,Num_or_com), 
	{append(Com,Num_or_com,Value)}.

read_integer0([Int]) -->
	[integer(Int)].

read_integer0([Hex]) -->
	[hex(Hex)].

%%%%%%%%%%%%%%%%
read_image(In,Out,Value) -->
	[integer(Im)],
	fillout(In,Out,Comment),
	{(Comment == []
	->
	Value = App
	;
	Value = [App])},
	{append([Im],Comment,App)}.

read_image(In,Out,[Im]) -->
	at_least_one(In,In0,Com),
	read_image(In0,Out,Rest),
	{append(Com,Rest,Im)}.
