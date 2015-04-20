:- module(_,[string2term/2],[]).

:- use_module(library(lists), [length/2]).

string2term(String,Term):-
	parse_term(String,Term, _).

parse_term([],'',[]).
parse_term([C|String0],Term,String):-
	parse_term0(C,String0,Term,String).
	
parse_term0(0'\s,String0,Term,String):- % space
        parse_term(String0,Term,String).
parse_term0(0'[,String0,Term,String):- !,
	parse_args0(String0,Term,[0']|String]).
parse_term0(0'( , _, _, _):- !, fail.
parse_term0(0') , _, _, _):- !, fail.
parse_term0(0'] , _, _, _):- !, fail.
parse_term0(0', , _, _, _):- !, fail.
parse_term0(C0,String0,Term,String):-
        parse_functor(C0,String0,FunctorStr,String1),
        ( length(FunctorStr, L), L > 500 ->
            Term = FunctorStr,
            String = String1
        ;
            name(Functor,FunctorStr),
            parse_args(String1,Args,String),
            Term=..[Functor|Args]
        ).

parse_functor(0'\s,String,[],[0'\s|String]).
parse_functor(0'( ,String,[],[0'( |String]).
parse_functor(0') ,String,[],[0') |String]).
parse_functor(0'[ ,String,[],[0'[ |String]).
parse_functor(0'] ,String,[],[0'] |String]).
parse_functor(0', ,String,[],[0', |String]).
parse_functor(0'' ,[C|String0],Functor,String):- !,
	parse_quoted(C,String0,Functor,String).
parse_functor(C, String,[C|Functor],String1):-
        parse_functor_(String,Functor,String1).

parse_functor_([], [], []).
parse_functor_([C|String],Functor,String1):-
	parse_functor(C,String,Functor,String1).

%parse_args([],[],[]).
parse_args([0'(|String0],Args,String):- !,
	parse_args0(String0,Args,[0')|String]).
parse_args(String,[],String).

parse_args0(String0,[Arg|Args],String):-
	parse_term(String0,Arg,String1), !,
	parse_args1(String1,Args,String).
parse_args0(String, [], String).

parse_args1([0'\s|String0],Args,String) :- !,
        parse_args1(String0,Args,String).
parse_args1([0', |String0],[Arg|Args],String):- !,
	parse_term(String0,Arg,String1),
	parse_args1(String1,Args,String).
parse_args1(String,[],String).

parse_quoted(0'',[0'',C|String0],[0''|Functor],String) :- !,
        parse_quoted(C,String0,Functor,String).
parse_quoted(0'',String,[],String) :- !.
parse_quoted(C1, [C|String0],[C1|Functor],String) :-
        parse_quoted(C,String0,Functor,String).
