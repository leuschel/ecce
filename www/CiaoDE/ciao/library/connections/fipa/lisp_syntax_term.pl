
:- module(lisp_syntax_term,[lisp_like/3],[dcg]).

:- use_module(library('pillow/pillow_aux')).
:- use_module(library(lists),[append/3]).

/*
  Encoding of named term structures of undefined number of arguments
  in strings with LISP-like syntax

  exp --> ( term arg* )    is    term( [ arg ] )
        | term                   
        | "anything"             

  arg --> :term exp        is    term = exp
        | exp

*/

lisp_like(Term,Str0,Str1):-
	var(Term), !,
	str2term(Term,Str0,Str1).
lisp_like(Term,Str0,Str1):-
	nonvar(Term),
	term2str(Term,Str0,Str1).

str2term(Term) --> sep0, str_lisp_exp(Term), sep0.

str_lisp_exp(Term) -->
	"(", !,
	sep0,
	str_lisp_term(Name),
	str_lisp_arg_list(Args),
	sep0,
	")",
	{ Args=[]
	-> Term=Name
	 ; functor(Term,Name,1),
	   arg(1,Term,Args)
	}.
str_lisp_exp(Name,In0,In1):-
	append(""""||Str,""""||In1,In0), !,
	append(""""||Str,"""",Name).
str_lisp_exp(Name) -->
	str_lisp_term(Name).

str_lisp_arg_list([Arg|Args]) -->
	sep,
	str_lisp_arg(Arg), !,
	str_lisp_arg_list(Args).
str_lisp_arg_list([]) --> "".

str_lisp_arg(Name=Value) -->
	":", !,
	str_lisp_term(Name),
	sep,
	str_lisp_exp(Value).
str_lisp_arg(Arg) -->
	str_lisp_exp(Arg).

str_lisp_term(Name,In0,In1):-
	until_sep(In0,Str,In1),
	Str=[_|_],
	atom_codes(Name,Str).

until_sep([X|In0],Str,In1):-
	sep_code(X), !,
	Str = [],
	In1 = [X|In0].
until_sep([X|In0],[X|Str],In1):-
	until_sep(In0,Str,In1).
until_sep([],[],[]).

sep_code(32).
sep_code(10).
sep_code(13).
sep_code(40).
sep_code(41).
sep_code(9).

sep --> sp, sep0.

sp --> http_sp.
sp --> http_crlf.

sep0 --> sp, !, sep0.
sep0 --> "".

term2str(Term) --> term_lisp_exp(Term).

term_lisp_exp(Term) -->
	{ atomic(Term) }, !,
	term_lisp_term(Term).
term_lisp_exp(Term) -->
	term_lisp_content(Term), !.
term_lisp_exp(Term) -->
	{ functor(Term,Name,1),
	  arg(1,Term,Args) },
	"(",
	term_lisp_term(Name),
	term_lisp_arg_list(Args),
	")".

term_lisp_arg_list([Arg|Args]) -->
	mysep,
	term_lisp_arg(Arg), !,
	term_lisp_arg_list(Args).
term_lisp_arg_list([]) --> "".

term_lisp_arg(Name=Value) --> !,
	":",
	term_lisp_term(Name),
	mysep,
	term_lisp_exp(Value).
term_lisp_arg(Arg) -->
	term_lisp_exp(Arg).

term_lisp_term(Name,In0,In1):-
	name(Name,Str),
	term_lisp_content(Str,In0,In1).

term_lisp_content(Str,In0,In1):-
	append(Str,In1,In0).

mysep([X|L],L):- sep_code(X), !.
