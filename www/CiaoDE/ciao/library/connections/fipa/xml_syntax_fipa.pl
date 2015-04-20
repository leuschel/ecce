
:- module(xml_syntax_fipa,[xml_like/2],[]).

:- use_module(library('pillow/html'),[xml2terms/2,url_info/2]).
%% :- use_module(library('pillow/pillow_aux')).
%% :- use_module(library(lists),[append/3]).

/* FIPA0024 Message Representation (according to SC00071E)
 */

xml_like(Term,Str):-
	var(Term), !,
	xml2terms(Str,Xml),
	message2term(Xml,Term).
xml_like(Term,Str):-
	nonvar(Term),
	term2message(Term,Str).

message2term(Xml,Term):-
	member(env(Fipa,Attr,More),Xml),
	case_insensitive("fipa_message",Fipa),
	member(act=Act,Attr),
	messageType(Type,Act,[]),
	functor(Term,Type,1),
	arg(1,Term,Params),
	env2params(More,Params).

env2params(More,More).




case_insensitive(Word,X):-
	atom_codes(X,Str),
	replace_the_name(Str,Word).

term2message(_Term,"").
