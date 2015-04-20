:- module(parser_util, [
	at_least_one/4,
	at_least_one/5,
	fillout/4,
	fillout/5,
	create_node/3,
	create_field/3,
	create_field/4,
	create_field/5,
	create_directed_field/5,
	correct_commenting/4,
	create_parse_structure/1,
	create_parse_structure/2,	
	create_parse_structure/3,
	create_environment/4,
	insert_comments_in_beginning/3,
	get_environment_name/2,
	get_environment_type/2,
	get_row_number/2,
	add_environment_whitespace/3,
	get_indentation/2,
	inc_indentation/2,
	dec_indentation/2,
	add_indentation/3,
	reduce_indentation/3,
	push_whitespace/3,
	push_dictionaries/3,
	get_parsed/2,
	get_environment/2,
	inside_proto/1,
	get_dictionaries/2,
	strip_from_list/2,
	strip_from_term/2,
	strip_clean/2,
	strip_exposed/2,
	strip_restricted/2,
	strip_interface/2,
	set_parsed/3,
	set_environment/3,
	%set_dictionaries/3,
	insert_parsed/3,
	reverse_parsed/2,
	stop_parse/2,
	look_first_parsed/2,
	get_first_parsed/3,
	remove_code/3,
	look_ahead/3
	],[dcg]).

:- include(library(assertions)).
:- include(library(basicmodes)).
:- include(library(types)).

:- include(library(iso)).
:- use_module(library(iso_byte_char)).
:- use_module(library(basicprops)).
:- use_module(library(lists)).

:- use_module(dictionary_tree).

%%%:- use_module(internal_types).
%%%:- use_module(i_o).

:- set_prolog_flag(multi_arity_warnings, off).
%%:- discontiguous([token_read/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stop_parse(S,S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_node(NodeTypeId,Parse_structure,Node) :-
	get_parsed(Parse_structure,Guts),
	Node =.. [NodeTypeId,Guts].

create_field(FieldNameId, Arguments, Field) :-
	%Old_field =.. [_Old_name|Arguments],
	Field =.. [FieldNameId|Arguments].

create_field(FieldNameId, [Arg], Field) :-
	extract_arguments_from_list(Arg, Arguments),
	Field =.. [FieldNameId,Arguments].

create_field(FieldAccess,FieldType,FieldId, Field) :-
	Field =.. [FieldAccess,FieldType,FieldId].

create_field(FieldAccess,FieldType,FieldId,FieldValue, Field) :-
	Field =.. [FieldAccess,FieldType,FieldId,FieldValue].

create_directed_field(FieldAccess,FieldType,FieldId0,FieldId1, Field) :-
	Field =.. [FieldAccess,FieldType,FieldId0,'IS',FieldId1].

%%%%%%%%%%%%%%%%
extract_arguments_from_list(Arg, Arguments) :-
	extract_arguments_from_list(Arg, [], Arguments).

extract_arguments_from_list([Old_field|Rest], In, Result) :-
	Old_field =.. [_Old_name|Arguments],
	append(In, Arguments, Out),
	extract_arguments_from_list(Rest, Out, Result).

extract_arguments_from_list([], Arguments, Arguments).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_NodeName(Node,NodeName) :-
	Node =.. [NodeName,_Guts].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_parse_structure(parse([],Env,Dictionaries)) :-
	create_dictionaries(Dictionaries),
	create_whitespace_environment(Env).

create_parse_structure(parse(_Parsed,Env,Dic),parse([],Env,Dic)).

create_parse_structure(Parsed,parse(Parsed,E,D)) :-
	create_dictionaries(D),
	create_whitespace_environment(E).

create_parse_structure(Parsed,parse(_Parsed,Env,Dic),parse(Parsed,Env,Dic)).



create_environment(parse(_,env(_,_,W),_),'PROTO',Name,
	           env('PROTO',Name,W)).

create_environment(parse(_,env(_,_,W),_),'EXTERNPROTO',Name,
	           env('EXTERNPROTO',Name,W)).

create_environment(parse(_,env(_,_,W),_),'DEF',Name,
	           env('DEF',Name,W)).



create_whitespace_environment(env(_,_,Ws)) :-
	create_whitespace(Ws).

create_whitespace(ws(0,0)).
create_whitespace(White,Indent,ws(White,Indent)).


get_parsed(parse(P,_,_),P).
get_environment(parse(_,E,_),E).
get_dictionaries(parse(_,_,D),D) :-
	!.

set_environment(E,parse(P,_,D),parse(P,E,D)).

get_environment_name(env(_Type,Name,_Ws),Name).
get_environment_type(env(Type,_Name,_Ws),Type).
get_environment_whitespace(env(_T,_N,W),W).

set_environment_whitespace(W,env(T,N,_W_old),env(T,N,W)).

set_environment_whitespace(W,parse(P,E,D),parse(P,E_new,D)) :-
	set_whitespace(W,E,E_new).

set_whitespace(W,env(T,N,_),env(T,N,W)).

set_parsed(parse(_,E,D),P,parse(P,E,D)).

set_dictionaries(D,parse(P,E,_),parse(P,E,D)) :-
	is_dictionaries(D).

push_dictionaries(Trash,In,Out) :-
	get_dictionaries(Trash,D),
	set_dictionaries(D,In,Out).

get_environment_whitespace_row(env(_T,_N,W),R) :-
	get_whitespace_row(W,R).

get_row_number(parse(_,E,_),Row) :-
	get_environment_whitespace_row(E,Row).

get_whitespace_row(ws(R,_Ind),R).
set_whitespace_row(R,ws(R,_Ind)).
set_whitespace_row(R,ws(_OldRow,Ind),ws(R,Ind)).

get_indentation(ws(_R,Ind),Ind).
get_indentation(Parse,Ind) :-
	get_environment(Parse,Env),
	get_environment_whitespace(Env,Ws),
	get_indentation(Ws,Ind).

set_indentation(Ind,ws(_R,Ind)).
set_indentation(Ind,ws(R,_OldInd),ws(R,Ind)).

%%%%%%%%%%%%%%%%

increase_row(Env,Inc,Out) :-
	get_environment_whitespace(Env,Ws),
	get_whitespace_row(Ws,Row),
	New is Row + Inc,
	set_whitespace_row(New,Ws,White_new),
	set_whitespace(White_new,Env,Out).	

add_environment_whitespace(In,White,Out) :-
	count_row(White,Rows),
	add_environment_whitespace_row(In,Rows,Out).
	
add_environment_whitespace_row(In,Rows,Out) :-
	get_environment(In,Env),
	increase_row(Env,Rows,New_env),
	set_environment(New_env,In,Out).
	
push_whitespace(Trash,Save,Out) :-
	get_environment(Trash,Env),
	get_environment_whitespace(Env,Ws),
	get_whitespace_row(Ws,R0),
	get_indentation(Ws,Indent),
	add_indentation(Save,Indent,Save0),	
	add_environment_whitespace_row(Save0,R0,Out).

%%%%%%%%%%%%%%%%
add_environment_indentation(Env,Add,Out) :-
	get_environment_whitespace(Env,Ws),
	get_indentation(Ws,Indent),
	New_indent is Indent + Add,
	set_indentation(New_indent,Ws,New_ws),
	set_environment_whitespace(New_ws,Env,Out).

reduce_environment_indentation(Env,Reduce,Out) :-
	get_environment_whitespace(Env,Ws),
	get_indentation(Ws,Indent),
	( Indent >= Reduce
	->New_indent is Indent - Reduce
	; New_indent = 0
	),
	set_indentation(New_indent,Ws,New_ws),
	set_environment_whitespace(New_ws,Env,Out).
	
%%%%%%%%%%%%%%%%
add_indentation(In,Add,Out) :-
	get_environment(In,Env),
	add_environment_indentation(Env,Add,Env_new),
	set_environment(Env_new,In,Out).

reduce_indentation(In,Reduce,Out) :-
	get_environment(In,Env),
	reduce_environment_indentation(Env,Reduce,New_env),
	set_environment(New_env,In,Out).

inc_indentation(In,Out) :-
	add_indentation(In,1,Out).

dec_indentation(In,Out) :-
	reduce_indentation(In,1,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
inside_proto(In) :-
	get_environment(In,E),
	get_environment_type(E,Name),
	(  Name == 'PROTO'
        ;  Name == 'EXTERNPROTO'
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
correct_commenting(before,Comment,Term,Out) :-
	( Comment == []
	->Out = Term
	; append(Comment,[Term],Out)
	).

correct_commenting(after,Comment,Term,Out) :-
	( Comment == []
	->Out = Term
	; append([Term],Comment,Out)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
strip_from_list([],[]).

strip_from_list([E|Rest],Output) :-
	list(E),
	strip_from_list(E,E0),
	strip_from_list(Rest,R0),
	append([E0],R0,Output).

strip_from_list([E|Rest], Output) :-
	atomic(E),
	name(E,List),
	List = [35|_],
	strip_from_list(Rest,Output).

strip_from_list([E|Rest], [E|Output]) :-
	strip_from_list(Rest,Output).

strip_from_list(A,A).

strip_clean(Atom,Atom) :-
	atomic(Atom).

strip_clean(List,Clean) :-
	list(List),
	strip_from_list(List,Out),
	( list(Out)
	-> Out = [More],
	   strip_clean(More,Clean)
	;  Clean = Out
	).
	

strip_from_term(Term,Stripped) :-
	compound(Term),
	Term =.. [Head|List],
	strip_from_list(List,Str),
	Stripped =.. [Head|Str].


strip_interface(Interface,Stripped) :-
	strip_from_list(Interface,Pure),
	strip_interface0(Pure,Stripped).

strip_interface0([],[]).
strip_interface0([Node|Interface], [New|Pure]) :-
	strip_exposed(Node,New),
	strip_interface(Interface,Pure).

strip_interface0([Node|Interface], [New|Pure]) :-
	strip_restricted(Node,New),
	strip_interface(Interface,Pure).

strip_restricted(Field,New) :-
	Field =.. [Acc,Type,Id],
	strip_clean(Type,Type_new),
	strip_clean(Id,Id_new),
	New =.. [Acc,Type_new,Id_new].

strip_exposed(Field,New) :-
	Field =.. [Acc,Type,Id,Value],
	strip_clean(Type,Type_new),
	strip_clean(Id,Id_new),
	strip_from_list(Value,Value_new),
	New =.. [Acc,Type_new,Id_new,Value_new].




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
insert_parsed(Parsed,In,Out) :-
	get_parsed(In,Old_parsed),
	append(Parsed, Old_parsed, New_parsed),
	set_parsed(In,New_parsed,Out).

insert_comments_in_beginning(Com,In,Out) :-
	get_parsed(In,Old_parsed),
	append(Com, Old_parsed, New_parsed),
	set_parsed(In,New_parsed,Out).

reverse_parsed(Parsed,Reversed) :-
	get_parsed(Parsed,P),
	reverse(P,Rev),
	set_parsed(Parsed,Rev,Reversed).

look_first_parsed(In,First) :-
	get_parsed(In,[First|_More]).
	
get_first_parsed(In,Out,First) :-
	get_parsed(In,[First|More]),
	set_parsed(In,More,Out).

%If there is no more
get_first_parsed(In,In,[]) :-
	get_parsed(In,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
look_ahead(Name, [Ahead|Rest], [Ahead|Rest]) :-
	Ahead =.. [_Token,Name|_More].

%Otherwise there is no more input or parenthesis.
%look_ahead(Name, [Ahead|Rest], [Ahead|Rest]) :-
%	Ahead =.. [Name|_More].





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	



remove_code(Stop_sign) -->
	[Stop_sign].
	
remove_code(Stop_sign) -->
	[Skipped],
	{write(Skipped),nl},
	remove_code(Stop_sign).

remove_code(_Stop_sign) -->
	[].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
count_row(Whitespace,Rows) :-
	count_row(Whitespace,0,Rows).

count_row([],R,R).
count_row([Num|Rest],In,Result) :-
	( ( Num == 10; 
	    Num == 13 )
	->  Out is In + 1,
	    count_row(Rest,Out,Result)
	;
	    count_row(Rest,In,Result)
	).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Can catch zero or more of each.
fillout(In,Out,Fill) -->
	at_least_one(In,In0,C),
	fillout(In0,Out,C1),
	{append(C,C1,Fill)}.

fillout(In,In,[]) -->
	[].

fillout(In,Out) -->
	at_least_one(In,In0),
	fillout(In0,Out).

fillout(In,In) --> 
	[].

%%%%%%%%%%%%%%%%
at_least_one(In,Out) -->
	whitespace(In,In0),
	comment_more(In0,Out).

at_least_one(In,Out) -->
	comment(In,In0),
	whitespace_more(In0,Out).

%%%%%%%%%%%%%%%%
at_least_one(In,Out,Com) -->
	whitespace(In,Out),
	comment_more(Com).

at_least_one(In,Out,Com) -->
	comment(Com),
	whitespace_more(In,Out).


	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
comment(In,Out) -->
	[comment(Com)],
	{insert_parsed([Com],In,In0)},
	comment_more(In0,Out).

comment_more(In,Out) -->
	[comment(Com)],
	comment_more(More),
	{insert_parsed([Com|More],In,Out)}.

comment_more(In,In) -->
	[].

%%%%%%%%%%%%%%%%

comment(Com) -->
	[comment(C)],
	comment_more(More),
	{append([C],More,Com)}.

comment_more([C|More]) -->
	[comment(C)],
	comment_more(More).

comment_more([]) -->
	[].

%%%%%%%%%%%%%%%%
whitespace(In,Out) -->
	[whitespace(W)],
	whitespace_more(More),
	{append(W,More,WhiteSpace),
	 add_environment_whitespace(In,WhiteSpace,Out)}.

whitespace_more(White) -->
	[whitespace(W)],
	whitespace_more(More),
	{append(W,More,White)}.
	
whitespace_more([]) -->
	[].

whitespace_more(In,Out) -->
	[whitespace(W)],
	whitespace_more(More),
	{append(W,More,WhiteSpace),
	 add_environment_whitespace(In,WhiteSpace,Out)}.

whitespace_more(In,In) -->
	[].
