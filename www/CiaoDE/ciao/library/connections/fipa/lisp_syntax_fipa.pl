
:- module(lisp_syntax_fipa,[lisp_like/2,lisp_like/3],[dcg]).

:- use_module(library('pillow/html'),[url_info/2]).
:- use_module(library('pillow/pillow_aux')).
:- use_module(library(lists),[append/3]).

/* FIPA0024 Message Representation (according to SC00070I)

   Messages are terms of the form
		    MessageType( List( Param ) )

   MessageType is the same as FIPA but normalized

               normalized means lowercase and with the - replaced by _

   Param is a term of the form
		       param( Name, Argument )

   Name is an atom with the same parameter name as FIPA without : and 
         normalized (including user defined parameters)

   Argument is an atom except for the following FIPA tokens:
         String is a string (including the surrounding ")
         DateTime is a term of the form
	       date(Sign,Type,Day,Month,Year,Time) and
		  Time is Hour:(Min:(Sec:MilliSec))
                  Sign and Type are atoms
                       and all others are numbers
         URL is as in pillow
         AgentIdentifier is List( Param )
         AgentIdentifierSet is List( AgentIdentifier )
         AgentIdentifierSequence is List( AgentIdentifier )

Bug: " within String are not yet supported, nor bytesequences
Bug: the admitted Word is more general than FIPA
         (basically, anything up to a separator)
Bug: Number is not returned as a number
 */

lisp_like(Term,Str):- lisp_like(Term,Str,[]).

lisp_like(Term,Str0,Str1):-
	var(Term), !,
	message2term(Term,Str0,Str1).
lisp_like(Term,Str0,Str1):-
	nonvar(Term),
	term2message(Term,Str0,Str1).

message2term(Term) -->
	sep0,
	"(",
	messageType(Name),
	messageParameterList(mess,Args),
	sep0,
	")",
	sep0,
	{ functor(Term,Name,1),
	  arg(1,Term,Args)
	}.

messageParameterList(Pos,[Arg|Args]) -->
	sep,
	messageParameter(Pos,Arg), !,
	messageParameterList(Pos,Args).
messageParameterList(_,[]) --> "".

messageParameter(Pos,param(Name,Value)) -->
	":",
	name(Word),
	{ replace_the_name(Word,Str),
	  atom_codes(Name,Str),
	  parameterType(Pos,Name,Type)
	},
	sep0,
	messageParameterValue(Type,Value).

parameterType(mess,Name,Type):- messageParameterType(Name,Type).
parameterType(agent,Name,Type):- agentParameterType(Name,Type).

messageParameterType(sender,agent).
messageParameterType(receiver,agset).
messageParameterType(reply_to,agset).
messageParameterType(content,string).
messageParameterType(reply_by,date).
messageParameterType(in_reply_to,exp).
messageParameterType(reply_with,exp).
messageParameterType(language,exp).
messageParameterType(encoding,exp).
messageParameterType(ontology,exp).
messageParameterType(conversation_id,exp).
messageParameterType(protocol,word).
messageParameterType(Name,exp):- userDefinedParameter(Name).

messageParameterValue(agent,Value)  --> agentIdentifier(Value).
messageParameterValue(agset,Value)  --> agentIdentifierSet(Value).
messageParameterValue(exp,Value)    --> expression(Value).
messageParameterValue(word,Value)   --> word(Value).
messageParameterValue(string,Value) --> string(Value).
messageParameterValue(date,Value)   --> dateTime(Value).
messageParameterValue(urlseq,Value) --> urlSequence(Value).
messageParameterValue(agseq,Value)  --> agentIdentifierSeq(Value).

agentParameterType(name,word).
agentParameterType(addresses,urlseq).
agentParameterType(resolvers,agseq).
agentParameterType(Name,exp):- userDefinedParameter(Name).

userDefinedParameter("x_"||_).

expressionList([Arg|Args]) -->
	sep,
	expression(Arg), !,
	expressionList(Args).
expressionList([]) --> "".

expression(Value,In0,In1):-
	append(""""||Str,""""||In1,In0), !,
	append(""""||Str,"""",Value).
expression(Value) -->
	"(", !,
	sep0,
	expression0(Args,Value),
	expressionList(Args),
	sep0,
	")".
expression(Value) -->
	dateTime(Value), !.
expression(Value) -->
	word(Value).

expression0(Args,Value) -->
	expression(Arg), !,
	{ Value=[Arg|Args] }.
expression0(Args,Args) --> "".
 
agentIdentifierSeq(Args) -->
	"(",
	sep0,
	parse_case("sequence"),
	agentIdentifierList(Args),
	sep0,
	")".

agentIdentifierSet(Args) -->
	"(",
	sep0,
	parse_case("set"),
	agentIdentifierList(Args),
	sep0,
	")".

agentIdentifierList([Arg|Args]) -->
	sep,
	agentIdentifier(Arg), !,
	agentIdentifierList(Args).
agentIdentifierList([]) --> "".

agentIdentifier(Args) -->
	"(",
	sep0,
	parse_case("agent-identifier"),
	messageParameterList(agent,Args),
	sep0,
	")".

urlSequence(Args) -->
	"(",
	sep0,
	parse_case("sequence"),
	urlList(Args),
	sep0,
	")".

urlList([Arg|Args]) -->
	sep,
	url(Arg), !,
	urlList(Args).
urlList([]) --> "".

url(Info) -->
	name(Str),
	{ url_info(Str,Info) }.

string(Name,In0,In1):-
	append(""""||Str,""""||In1,In0), !,
	append(""""||Str,"""",Name).

dateTime(date(S,Type,Day,Month,Year,Hour:(Min:(Sec:Mill)))) -->
	sign(S),
        [Y1,Y2,Y3,Y4,M1,M2,D1,D2],
	"T",
        [H1,H2,N1,N2,S1,S2,L1,L2],
	type(Type),
        { number_codes(Day,[D1,D2]),
	  number_codes(Month,[M1,M2]),
	  number_codes(Year,[Y1,Y2,Y3,Y4]),
	  number_codes(Hour,[H1,H2]),
	  number_codes(Min,[N1,N2]),
	  number_codes(Sec,[S1,S2]),
	  number_codes(Mill,[L1,L2])
        }.

sign(+) --> "+", !, sep.
sign(-) --> "-", !, sep.
sign('') --> "".

type(Type) --> [C],
	{ member(C,"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"),
	  !,
	  atom_codes(Type,[C])
	}.
type('') --> "".

word(Name,In0,In1):-
	name(Str,In0,In1),
	atom_codes(Name,Str).

name(Str,In0,In1):-
	until_sep(In0,Str,In1),
	Str=[_|_].

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

replace_the_name([N0|Name0],[N1|Name1]):-
	replace_the_char(N0,N1),
	replace_the_name(Name0,Name1).
replace_the_name([],[]).

replace_the_char(45,95):- !.
replace_the_char(X,Y):- uplow([Y],[X]), !.
replace_the_char(X,X).

parse_case([]) --> "".
parse_case([X|Xs]) --> parse_uplow(X), !, parse_case(Xs).

parse_uplow(X) --> [X].
parse_uplow(X) --> { uplow([X],[Y]) }, [Y].

uplow("a","A").
uplow("b","B").
uplow("c","C").
uplow("d","D").
uplow("e","E").
uplow("f","F").
uplow("g","G").
uplow("h","H").
uplow("i","I").
uplow("j","J").
uplow("k","K").
uplow("l","L").
uplow("m","M").
uplow("n","N").
uplow("o","O").
uplow("p","P").
uplow("q","Q").
uplow("r","R").
uplow("s","S").
uplow("t","T").
uplow("u","U").
uplow("v","V").
uplow("w","W").
uplow("x","X").
uplow("y","Y").
uplow("z","Z").

term2message(_Term,Str0,Str0).
