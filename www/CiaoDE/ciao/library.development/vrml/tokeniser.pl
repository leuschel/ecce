:- module(tokeniser, [tokeniser/2,token_read/3,koll/0,koll2/0], [dcg]).

:- include(library(assertions)).
:- include(library(basicmodes)).
:- include(library(types)).

:- include(library(iso)).
:- use_module(library(iso_byte_char)).
:- use_module(library(basicprops)).
:- use_module(library(lists)).
:- use_module(error).
%%%:- use_module(internal_types).
%%%:- use_module(i_o).

:- set_prolog_flag(multi_arity_warnings, off).
:- discontiguous([token_read/3]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tokeniser(VRML, Tokens) :-
       	catch( tokeniser(Tokens_first, VRML, []), Msg, output_error(Msg)),
	remove_whitespace(Tokens_first, Tokens),
	!.

tokeniser([]) -->
	"".

tokeniser([Head|Tail]) -->
	token_read(Head),
	tokeniser(Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

remove_whitespace([],[]).
remove_whitespace([whitespace(_)|Rest], Ans) :-
	remove_whitespace(Rest, Ans).

remove_whitespace([Token|Rest],[Token|Ans]) :-
	remove_whitespace(Rest, Ans).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
token_read(string(String)) -->
	char_string_symbol(_),
	token_read_string(String_list),
	{ name(String, String_list) }.

token_read_string([C,B|Acc]) -->
	char_backslash(C),
	token_read_string_backslash(B),
	token_read_string(Acc).

token_read_string([]) -->
	char_string_symbol(_).

token_read_string([C|Acc]) -->
	[C], 
	 token_read_string(Acc).

%Special characters in a string, have to be treated specially.

token_read_string_backslash(C) -->
	char_backslash(C).

token_read_string_backslash(C) -->
	char_string_symbol(C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
token_read(comment(Comment)) -->
	token_read_comment(Comment_list),
	{ name(Comment,Comment_list) }.

token_read_comment([C|Com]) -->
	char_comment_symbol(C),
	token_read_comment_rest(Com).

%The comment only finnishes with new line or a return.

token_read_comment_rest([]) -->
	char_newline, 
	!.

token_read_comment_rest([]) -->
	char_return, 
	!.

token_read_comment_rest([C|Acc]) -->
	[C],
	token_read_comment_rest(Acc).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
token_read(id(Identifier)) -->
	token_read_id_big(Id_list),
	{ name(Identifier, Id_list) }.

token_read(id(Identifier)) -->
	token_read_id_normal(Id_list),
	{ name(Identifier, Id_list) }.

token_read_id_big([C|Acc]) -->
	char_upper_case(C),
	token_read_id(Acc).
	  
token_read_id_normal([C|Acc]) -->
	char_id_first(C),
	token_read_id(Acc).

token_read_id([C|Acc]) -->
	char_id_rest(C),
	token_read_id(Acc).

token_read_id([]) -->
	"",
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
token_read(exp(Exp)) -->
	token_read_exp_first(Exp_list),
	{ name(Exp, Exp_list) }.

token_read_exp_first([D|Rest]) -->
	char_digit(D),
	token_read_exp(Rest).

token_read_exp_first([N|Rest]) -->
	char_negation(N),
	token_read_exp_second(Rest).

token_read_exp_second(Rest) -->
	token_read_whitespaces(_),
	token_read_exp_second(Rest).

token_read_exp_second([D|Rest]) -->
	char_digit(D),
	token_read_exp(Rest).

token_read_exp([D|Rest]) -->
	char_digit(D),
	token_read_exp(Rest).

token_read_exp([D|Rest]) -->
	char_dot(D),
	token_read_exp_third(Rest).

token_read_exp(Exp) -->
	char_exp(_Sym),
	token_read_exp_exponent(Exp).

token_read_exp_third(Value) -->
	token_read_integer(Rest),
	char_exp(Sym),
	token_read_exp_exponent_dot(Exp),
	{ append(Rest,[Sym],Values0),
	  append(Values0,Exp,Value) }.



token_read_exp_exponent_dot(Exp) -->
	token_read_float_first(Exp).

token_read_exp_exponent_dot(Exp) -->
	token_read_integer_first(Exp).


%%%%%We want the exponent to be a number and therefore have to be a float base
token_read_exp_exponent([0'.,0'0,0'e|Exp]) -->
	token_read_float_first(Exp).

token_read_exp_exponent([0'.,0'0,0'e|Exp]) -->
	token_read_integer_first(Exp).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%







token_read(float(Float)) -->
	token_read_float_first(Int_list),
	{ name(Float, Int_list) }.

token_read_float_first([D|Rest]) -->
	char_digit(D),
	token_read_float(Rest).

token_read_float_first([N|Rest]) -->
	char_negation(N),
	token_read_float_second(Rest).

token_read_float_first([0'0,D|Rest]) -->
	char_dot(D),
	token_read_float_third(Rest).

token_read_float_first([C,D|Rest]) -->
	char_zero(C),
	char_dot(D),
	token_read_integer(Rest).

token_read_float_second(Rest) -->
	token_read_whitespaces(_),
	token_read_float_second(Rest).

token_read_float_second([D|Rest]) -->
	char_digit(D),
	token_read_float(Rest).

token_read_float_third([D|Rest]) -->
	char_digit(D),
	token_read_integer(Rest).

token_read_float([D|Rest]) -->
	char_digit(D),
	token_read_float(Rest).

token_read_float([D|Rest]) -->
	char_digit(D),
	token_read_float(Rest).

token_read_float([D|Rest]) -->
	char_dot(D),
	token_read_integer(Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
token_read(hex(Hex)) -->
	token_read_hex_first(Hex_list),
	{ name(Hex, Hex_list) }.

token_read_hex_first([C,M|Rest]) -->
	char_zero(C),
	char_hex_mark(M),
	!,
	token_read_hex_rest(Rest).

token_read_hex_first([N|Rest]) -->
	char_negation(N),
	token_read_hex_second(Rest).

token_read_hex_second(Rest) -->
	token_read_whitespaces(_),
	token_read_hex_second(Rest).

token_read_hex_second([C,M|Rest]) -->
	char_zero(C),
	char_hex_mark(M),
	!,
	token_read_hex_rest(Rest).

token_read_hex_rest([C|Rest]) -->
	char_hex(C),
	token_read_hex(Rest).

token_read_hex([C|Rest]) -->
	char_hex(C),
	token_read_hex(Rest).


token_read_hex([]) -->
	"",
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
token_read(integer(Int)) -->
	token_read_integer_first(Int_list),
	{ name(Int, Int_list) }.

token_read_integer_first([D|Rest]) -->
	char_digit(D),
	token_read_integer(Rest).

token_read_integer_first([N|Rest]) -->
	char_negation(N),
	token_read_integer_second(Rest).

token_read_integer_second(Rest) -->
	token_read_whitespaces(_),
	token_read_integer_second(Rest).

token_read_integer_second([D|Rest]) -->
	char_digit(D),
	token_read_integer(Rest).

token_read_integer([D|Rest]) -->
	char_digit(D),
	token_read_integer(Rest).

token_read_integer([]) -->
	"",
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
token_read(symbol(Symbol)) -->
	token_read_symbol(Symbol_list),
	{ name(Symbol, Symbol_list) }.

token_read_symbol([Symbol]) -->
	[Symbol],
	{ char_symbols_allowed(Sym),
        member(Symbol,Sym) }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
token_read(whitespace(Whitespace_list)) -->
	token_read_whitespaces(Whitespace_list).

token_read_whitespaces([C|Acc]) -->
	char_whitespace(C),
	!,
	token_read_whitespaces_more(Acc).

token_read_whitespaces_more([Head|Tail]) --> 
	char_whitespace(Head),
	token_read_whitespaces_more(Tail).

token_read_whitespaces_more([]) --> 
	"",
	!.

char_whitespace(32) --> [32],!. %space
char_whitespace(44) --> [44],!. %comma
char_whitespace(13) --> [13],!. %return
char_whitespace(10) --> [10],!. %newline
char_whitespace(9)  --> [9], !. %tab


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
token_read(parenthesis_node_open) -->
	char_parenthesis_node_open.

token_read(parenthesis_node_close) -->
	char_parenthesis_node_close.

token_read(parenthesis_list_open) -->
	char_parenthesis_list_open.

token_read(parenthesis_list_close) -->
	char_parenthesis_list_close.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
token_read([],Input,_) :-
	atom_codes(Atom,Input),
	error_vrml(tokeniser(Atom)).	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
char_id_first(Code) -->
	[C],
	(
	    { character_exceptions_first(Exc),
	      member(C,Exc) }
	->
	    {fail}
	;
	    {Code = C }
	).

char_id_rest(Code) -->
	[C], 
	(
	    { character_exceptions_rest(Exc),
	      member(C,Exc)}
	->
	    {fail}
        ;
	    {Code = C }
        ).

%%%%%%%%%%%%%%%%
char_letter(C) --> 
	char_lower_case(C).

char_letter(C) --> 
	char_upper_case(C).

%%%%%%%%%%%%%%%%

char_lower_case(C) -->
	[C], 
	{C >= 0'a, C =< 0'z}.
	
char_upper_case(C) -->
	[C], 
	{C >= 0'A, C =< 0'Z}.

%%%%%%%%%%%%%%%%

char_digit(C) -->
	[C],
	{ C >= 0'0, C =< 0'9 }.

char_hex(C) -->
	[C],
	{ C >= 0'A, C =< 0'F }.

char_hex(C) -->
	[C],
	{ C >= 0'a, C =< 0'f }.

char_hex(C) -->
	[C],
	{ C >= 0'0, C =< 0'9 }.

char_negation(0'-) --> "-".

char_dot(0'.) --> ".".

char_zero(0'0) --> "0".

char_exp(0'e) --> "e".

char_hex_mark(0'x) --> "x".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

char_string_symbol(0'") --> """".

char_backslash(0'\\) --> [41].

char_comment_symbol(0'#) --> "#".

char_parenthesis_node_open --> "{".

char_parenthesis_node_close --> "}".

char_parenthesis_list_open --> "[".

char_parenthesis_list_close --> "]".

char_newline --> [10].
char_return --> [13].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
char_symbols_allowed([46]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
character_exceptions_first_hex([30,31,32,33,34,35,36,37,38,39,0,1,2,3,4,5,6,7,8,9,a,b,c,d,e,f,10,11,12,13,14,15,16,17,18,19,'1a','1b','1c','1d','1e','1f',20,22,23,27,'2b','2c','2d','2e','5b','5c','5d','7b','7d']).

character_exceptions_rest_hex([0,1,2,3,4,5,6,7,8,9,a,b,c,d,e,f,10,11,12,13,14,15,16,17,18,19,'1a','1b','1c','1d','1e','1f',20,22,23,27,'2c','2e','5b','5c','5d','7b','7d']).

character_exceptions_first([43,45,48,49,50,51,52,53,54,55,56,57,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,34,35,39,44,46,91,92,93,123,125]).

character_exceptions_rest([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,34,35,39,44,46,91,92,93,123,125]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hex2dec(Hex,Dec) :-
	name(Hex, HexList),
	length(HexList, Length),
	Nr is Length - 1,
	hexlist2dec(HexList, Nr, Dec).

hexlist2dec(HexList, Nr, Dec):-
	hexlist2dec(HexList, Nr, 0, Dec).

hexlist2dec([], _, Acc, Acc).

hexlist2dec([H|L], Nr, Acc, Dec) :-
	H >= 0'a,
	H =< 0'f,
	Val is (( H - 87 ) * ( 1 << (Nr * 4 ))),
	Acc2 is Acc + Val,
	Nr2 is Nr - 1,
	hexlist2dec(L, Nr2, Acc2, Dec).

hexlist2dec([H|L], Nr, Acc, Dec) :-
	H >= 0'A,
	H =< 0'F,
	Val is (( H - 55 ) * ( 1 << (Nr * 4 ))),
	Acc2 is Acc + Val,
	Nr2 is Nr - 1,
	hexlist2dec(L, Nr2, Acc2, Dec).

hexlist2dec([H|L], Nr, Acc, Dec) :-
	H >= 0'0,
	H =< 0'9,
	Val is (( H - 48) * ( 1 << (Nr * 4 ))),
	Acc2 is Acc + Val,
	Nr2 is Nr - 1,
	hexlist2dec(L, Nr2, Acc2, Dec).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

koll :-
	character_exceptions_first_hex(F),
	koll(F).

koll2 :-
	character_exceptions_rest_hex(R),
	koll(R).
	
koll([]).
koll([H|L]):-
	hex2dec(H,D),
	write(H),
	put_code(32),
	put_code(D),
	put_code(9),
	put_code(10),
	koll(L).
