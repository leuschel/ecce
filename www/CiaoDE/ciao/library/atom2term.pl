:- module(atom2term,
	[ atom2term/2,
	  string2term/2,
          parse_term/3
	],
	[ assertions,
	  basicmodes
	]).

:- comment(title,"Atom to term conversion").

:- comment(author,"Francisco Bueno").
:- comment(author,"Daniel Cabeza").

:- comment(module, "This module implements the predicates involved in
   the atom to term conversion.").

:- comment(bug,"This is just a quick hack written mainly for parsing 
   daVinci's messages. There should be a call to the standard reader
   to do this!").

:- pred atom2term(+Atom,-Term) # "Convert an atom into a term.
   @var{Atom} is an atom, but must have term syntax.  @var{Term} is a
   term resulting from parsing @var{Atom} char by char.".

atom2term(Atom,Term):-
	atom_codes(Atom,String),
	parse_term(String,Term, _), !.

:- pred string2term(+String,-Term) # "Same as @pred{atom2term/2} but
   first argument is a string (containing a term).".

string2term(String,Term):-
	parse_term(String,Term, _), !.

:- pred parse_term(+String, -Term, ?Dummy)
      # "@var{String} is parsed into @var{Term} upto @var{Dummy}
         (which is the non-parsed rest of the list).".

parse_term([],'',[]).
parse_term([C|String0],Term,String):-
	parse_term0(C,String0,Term,String).
	
parse_term0(0'\s,String0,Term,String):- % space
        parse_term(String0,Term,String).
parse_term0(0'[,String0,Term,String):- !,
	parse_args0(String0,Term,[0']|String]).
parse_term0(0'",String0,Term,String):- !,
	parse_string(String0,Str,String),
	atom_codes(Term,Str).
parse_term0(0'( , _, _, _):- !, fail.
parse_term0(0') , _, _, _):- !, fail.
parse_term0(0'] , _, _, _):- !, fail.
parse_term0(0', , _, _, _):- !, fail.
parse_term0(C0,String0,Term,String):-
        parse_functor(C0,String0,FunctorStr,String1),
        name(Functor,FunctorStr),
	parse_args(String1,Args,String),
	Term=..[Functor|Args].

parse_functor(0'\s,String,[],[0'\s|String]).
parse_functor(0'( ,String,[],[0'( |String]).
parse_functor(0') ,String,[],[0') |String]).
parse_functor(0'[ ,String,[],[0'[ |String]).
parse_functor(0'] ,String,[],[0'] |String]).
parse_functor(0', ,String,[],[0', |String]).
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

parse_string([],[],[]).
parse_string([C|String],List,String1):-
	parse_string0(C,String,List,String1).

parse_string0(0'",String,[],String):- !.
parse_string0(C,String,[C|List],String1):-
	parse_string(String,List,String1).
