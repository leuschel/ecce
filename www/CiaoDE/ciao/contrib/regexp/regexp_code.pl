:- module(regexp_code,
        [match_posix/3,
	 match_shell/3,
	 replace_first/4,
	 replace_all/4,
	 match/3], [assertions, dcg]).

:- use_module(library(lists),
	[
	    append/3
	]).

:- op(   25,   fy,   (^)).

%%%VER  SI DCG SON EFICIENTES!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%creo que en match si, pero en posix_to_struct no!!!!!!!!!!!!!!!!!!!
%si no se reconoce un solo atomo --> siempre encerrado en lista, revisar
%in y not_in, para no formar lista si llega atomo duplican reglas --> buscar unir. Este problema ver en todo el codigo
%DCG en shell/posix_to_struct?????????????
%hacer expre.reg de sustitucion
%hacer todo lo del find_all
%parametrizar el prefijo más largo/corto
%hacer el encage con ~=
%caracter de escape para estructuras
%hacer automatas
%hacer expresiones regulares tippo arbol
%.* --> si queremos que siempre sea el mismo caracter, el usuario pone "X*" en match_struct, sino cada uno distinto vale con .*
%meter todo lo que falta en relación a posix según www.inforg.uniovi.es/Documentos/ManualUnix.pdf 

%%not_in e in deben llevar una lista dentro
%is_or y compleat_or más curraos!!!!!!!!!!!!!!!!!!  --> ya esta, falta ver eso de los restos!!!

:- comment(title,"Pattern (regular expresion) matching").

:- comment(author,"The Clip Group").

:- comment(module,"This library provides facilities for matching
   strings and terms against @index{patterns} (i.e., @index{regular
   expressions}).").



replace_first(IN,Old,New,Resul) :- 
	posix_to_struct(Old,[],Pattern,[]),
	replace(IN,first,Pattern,New,Resul).

replace_all(IN,Old,New,Resul) :- 
	posix_to_struct(Old,[],Pattern,[]),
	replace(IN,all,Pattern,New,Resul).

replace([],_,_,_,[]).
replace([IN|RestIN],Type,Pattern,New,Resul) :- 
	(
	    match(Pattern,[IN|RestIN],Rest) ->
	    construct(Type,New,Pattern,Rest,Resul)
	;
	    replace(RestIN,Type,Pattern,New,ResulRest),
	    Resul = [IN|ResulRest]
	). 

construct(first,Preff,_,Rest,Resul) :- append(Preff,Rest,Resul).
construct(all,Preff,Pattern,Rest,Resul) :- 
	replace(Rest,all,Pattern,Preff,RestConvert),
	append(Preff,RestConvert,Resul).


match_shell(Exp, IN, Rest) :-
	shell_to_struct(Exp,Pattern,yes),!,
	match(Pattern, IN, Rest).

shell_to_struct([],[],no).
shell_to_struct([0'*|Rest],Pattern,First) :-
        !, shell_to_struct(Rest,Pattern2,no),
	(
	    First == no ->
	    Pattern = [*(any)|Pattern2]
	;
	    Pattern = [not_in_cond([0'.],simple),*(any)|Pattern2]
        ).
shell_to_struct([0'?|Rest],[Preff|Pattern],First) :-
        !, shell_to_struct(Rest,Pattern,no),
	(
	    First == no ->
	    Preff = inter(any,0,1)
	;
	    Preff = not_in_cond([0'.],double)
	).
shell_to_struct([0'\\,Ch|RestIn],[Ch|Pattern],_) :-
        !, shell_to_struct(RestIn,Pattern,no).
shell_to_struct([0'{|RestIn],[or(Set)|Pattern],_) :-
        !, keys(RestIn,Set,RestKey),
        shell_to_struct(RestKey,Pattern,no).
shell_to_struct([Ch|RestIn],[Ch|Pattern],_) :-
        !, shell_to_struct(RestIn,Pattern,no).

keys([0'\\,Ch,0',|Rest],[Ch|Set],RestOut) :- 
	keys(Rest,Set,RestOut). 
keys([0'\\,Ch,0'}|Rest],[Ch],Rest).
keys([Ch,0',|Rest],[Ch|Set],RestOut) :- 
	Ch =\= 0'\\, keys(Rest,Set,RestOut). 
keys([Ch,0'}|Rest],[Ch],Rest) :- 
	Ch =\= 0'\\.


match_posix(Exp, IN, Rest) :-
	posix_to_struct(Exp,[],Pattern,[]),!,
	match(Pattern, IN, Rest).

posix_to_struct(Exp,End,Pattern,RestPattern) :-
	next_element(Exp,_,no,Pattern2,RestPattern2),
	compleat_pattern(End,RestPattern2,yes,Pattern2,Pattern,RestPattern).

compleat_pattern(End,End,yes,Pattern,Pattern,End) :- !.
compleat_pattern(End,End,no,Pattern,[Pattern],End) :- !.
compleat_pattern(End,[0'||RestExp],First,Pattern2,Pattern,RestPattern) :- 
	next_element(RestExp,_,no,Pattern3,RestPattern2),
	compleat_pattern(End,RestPattern2,no,Pattern3,Pattern4,RestPattern),
	test_first(First,Pattern2,Pattern4,Pattern).

test_first(yes,Pattern2,Pattern3,or([Pattern2|Pattern3])).
test_first(no,Pattern2,Pattern3,[Pattern2|Pattern3]).


put_preff(yes,Preff,Rest,[Preff|Rest]).
put_preff(no,_,Pattern,Pattern).

next_element([],Preff,yes,[Preff],[]).
next_element([],_,no,[],[]).
next_element([0'||Rest],Preff,yes,[Preff],[0'||Rest]) :- !.

next_element([0'*|Rest],Preff,YesPreff,Pattern,RestOut) :- 
	!, YesPreff = yes,         %debe venir unn prefijo a la fuerza
        next_element(Rest,*(Preff),yes,Pattern,RestOut).

next_element([0'+|Rest],Preff,YesPreff,Pattern,RestOut) :- 
	!, YesPreff = yes,
        next_element(Rest,+(Preff),yes,Pattern,RestOut).

next_element([0'\\,Ch|Rest],Preff,YesPreff,Pattern,RestOut) :-
        !, next_element(Rest,Ch,yes,Pattern2,RestOut),
	put_preff(YesPreff,Preff,Pattern2,Pattern).

next_element([0'.|Rest],Preff,YesPreff,Pattern,RestOut) :-
        !, next_element(Rest,any,yes,Pattern2,RestOut),   %los cortes evitan que entre en la ultima -->taria mal
	put_preff(YesPreff,Preff,Pattern2,Pattern).

next_element([0'(|RestIn],Preff,YesPreff,Pattern,RestOut) :-  %poner lo de result!!!!!!!!!!!!!!!!!!!!!!!!!! --> eliminar lista vacia
        !, next_element(RestIn,_,no,PatternParent2,RestTemp),
	compleat_pattern([0')|_],RestTemp,yes,PatternParent2,PatternParent,[0')|RestTemp2]),
	next_element(RestTemp2,PatternParent,yes,Pattern2,RestOut),
	put_preff(YesPreff,Preff,Pattern2,Pattern).

next_element([0')|Rest],Preff,YesPreff,[Preff],RestOut) :- 
	!, YesPreff = yes, RestOut = [0')|Rest].

next_element([0'[, 0'^|RestIn],Preff,YesPreff,Pattern,RestOut) :-
        !, read_chars(RestIn,no,_,Chars,Rest),
        next_element(Rest,not_in(Chars),yes,Pattern2,RestOut),
	put_preff(YesPreff,Preff,Pattern2,Pattern).

next_element([0'[|RestIn],Preff,YesPreff,Pattern,RestOut) :-
        !, read_chars(RestIn,no,_,Chars,Rest),
        next_element(Rest,in(Chars),yes,Pattern2,RestOut),
	put_preff(YesPreff,Preff,Pattern2,Pattern).

next_element([0'{|Rest],Preff,YesPreff,Pattern,RestOut) :- 
	!, YesPreff = yes,
	read_number(Rest,0,N,[Ch|Rest2]),
	read_number2(Ch,Rest2,N,M,Rest3),
        next_element(Rest3,inter(Preff,N,M),yes,Pattern,RestOut).

next_element([X|Rest],Preff,YesPreff,Pattern,RestOut) :-
        next_element(Rest,X,yes,Pattern2,RestOut),
	put_preff(YesPreff,Preff,Pattern2,Pattern).


read_chars([0']|Rest],YesPreff,Preff,[Preff],Rest) :- 
	!, YesPreff == yes.

read_chars([0'\\,Ch,0'-,0'\\,Ch2|Rest],YesPreff,Preff,Chars,RestOut) :- 
	!, Ch < Ch2, read_chars(Rest,yes,rank(Ch,Ch2),RestChars,RestOut),
	put_preff(YesPreff,Preff,RestChars,Chars).
read_chars([0'\\,Ch,0'-,Ch2|Rest],YesPreff,Preff,Chars,RestOut) :- 
	!, Ch < Ch2, read_chars(Rest,yes,rank(Ch,Ch2),RestChars,RestOut),
	put_preff(YesPreff,Preff,RestChars,Chars).
read_chars([0'\\,Ch|Rest],YesPreff,Preff,Chars,RestOut) :- 
	!, read_chars(Rest,yes,Ch,RestChars,RestOut),
	put_preff(YesPreff,Preff,RestChars,Chars).
read_chars([Ch,0'-,0'\\,Ch2|Rest],YesPreff,Preff,Chars,RestOut) :- 
	!, Ch < Ch2, read_chars(Rest,yes,rank(Ch,Ch2),RestChars,RestOut),
	put_preff(YesPreff,Preff,RestChars,Chars).
read_chars([Ch,0'-,Ch2|Rest],YesPreff,Preff,Chars,RestOut) :- 
	!, Ch < Ch2, read_chars(Rest,yes,rank(Ch,Ch2),RestChars,RestOut),
	put_preff(YesPreff,Preff,RestChars,Chars).
read_chars([0'[,0':,0'a,0'l,0'n,0'u,0'm,0':,0']|Rest],YesPreff,Preff,Chars,RestOut) :- 
	!, read_chars(Rest,yes,rank(0'0,0'9),RestChars,RestOut),
	put_preff(yes,rank(0'a,0'z),RestChars,RestChars2),
	put_preff(yes,rank(0'A,0'Z),RestChars2,RestChars3),
	put_preff(YesPreff,Preff,RestChars3,Chars).
read_chars([0'[,0':,0'a,0'l,0'p,0'h,0'a,0':,0']|Rest],YesPreff,Preff,Chars,RestOut) :- 
	!, read_chars(Rest,yes,rank(0'A,0'Z),RestChars,RestOut),
	put_preff(yes,rank(0'a,0'z),RestChars,RestChars2),
	put_preff(YesPreff,Preff,RestChars2,Chars).
read_chars([0'[,0':,0'd,0'i,0'g,0'i,0't,0':,0']|Rest],YesPreff,Preff,Chars,RestOut) :- 
	!, read_chars(Rest,yes,rank(0'0,0'9),RestChars,RestOut),
	put_preff(YesPreff,Preff,RestChars,Chars).
read_chars([0'[,0':,0'l,0'o,0'w,0'e,0'r,0':,0']|Rest],YesPreff,Preff,Chars,RestOut) :- 
	!, read_chars(Rest,yes,rank(0'a,0'z),RestChars,RestOut),
	put_preff(YesPreff,Preff,RestChars,Chars).
read_chars([0'[,0':,0'u,0'p,0'p,0'e,0'r,0':,0']|Rest],YesPreff,Preff,Chars,RestOut) :- 
	!, read_chars(Rest,yes,rank(0'A,0'Z),RestChars,RestOut),
	put_preff(YesPreff,Preff,RestChars,Chars).
read_chars([Ch|Rest],YesPreff,Preff,Chars,RestOut) :- 
	!, read_chars(Rest,yes,Ch,RestChars,RestOut),
	put_preff(YesPreff,Preff,RestChars,Chars).

digit(X) :- !, X > 47, !, X < 59.

read_number([D|Rest],Prev,N,RestOut) :- 
	(
	    digit(D) ->
	    Prev2 is Prev * 10,
	    Prev3 is Prev2 + D - 48,
	    read_number(Rest,Prev3,N,RestOut)
	;
	    N = Prev,
	    RestOut = [D|Rest]
	).
	
read_number2(0'},Rest,N,N,Rest).
read_number2(0',,Rest,_,N,RestOut) :-
	(
	    Rest = [0'}|RestOut] ->
	    N = inf
        ; 
	    read_number(Rest,0,N,[0'}|RestOut])
        ).

%ver si hay que poner cortes
match(^(X)) --> !, [X].

match([]) --> !, [].
match([E|Es]) --> !, match(E), match(Es).

match(*(X)) --> !, ([] ; match(X), match(*(X))).

match(+(X)) --> !, match(X), match(*(X)).
match(any) --> !, [_].

match(or([X|Xs])) --> !, (match(X) ; match(or(Xs))).

match(inter(X,N,M)) --> !, ({N == 0}, [] ; 
	                   {N == 0}, {M > 0}, match(X), {M2 is M - 1}, match(inter(X,0,M2)) ; 
                           {N > 0}, match(X), {N2 is N - 1,M2 is M - 1}, match(inter(X,N2,M2))).

match(not_in(Chars)) --> !, [Ch], {\+(contains(Chars,Ch))}.

match(in(Chars)) --> !, [Ch], {contains(Chars,Ch)}.

match(not_in_cond(Chars,Type),IN,Rest) :- !, not_in_cond(Chars,Type,IN,Rest).

match(Ch) --> [Ch].

not_in_cond(_,_,[],[]).
not_in_cond([],_,Rest,Rest).
not_in_cond([],double,[_|Rest],Rest).
not_in_cond([First|RestChars],Type,[Ch|Rest],RestOut) :-
	First =\= Ch,
	not_in_cond(RestChars,Type,[Ch|Rest],RestOut).

contains([rank(A,B)|_],Ch) :-
	Ch >= A,
	Ch =< B,
	!.
contains([Ch|_],Ch2) :-
	Ch == Ch2, !.
contains([_|Rest],Ch2) :-
	contains(Rest,Ch2).
