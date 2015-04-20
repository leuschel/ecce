:- module(persdbtr,[rewrite_program/1,rewrite/1]).

:- include(library(assertions)).
:- use_module(library(dec10_io)).
:- use_module(library(format)).
:- use_module(library(messages)).
:- use_module(library(lists)).
:- use_module(library('persdb_sql/pl2sql')).

rewrite_program(InNoPL):-
    atom_concat(InNoPL,'.pl',In),
    atom_concat(InNoPL,'_tr.pl',Out),
    see(In),
    tell(Out),
    format(":- multifile([relation/3,attribute/4]).\n",[]),
    format(":- data([relation/3,attribute/4]).\n\n",[]),
    myread(Term),
    rewrite(Term),
    told,
    seen.

rewrite(end_of_file):- 
    !.

rewrite((:- persistent(PrologDef,SQLDef,DBId))) :- 
    !,
    PrologDef =.. [PrologName | Types],
    SQLDef    =.. [TableName  | ArgNames],
    declare_table(PrologName,TableName, Types, ArgNames),
    functor(PrologDef,PrologName,Arity),
    functor(Consequence,PrologName,Arity),
    format("~q :- \n\tdb_call(~q,~q).\n\n",[Consequence,DBId,Consequence]),
    myread(Term),
    rewrite(Term).
    
%% rewrite((:-external_predicate(A/N),TableName,Types)):- !,
%%     functor(Head, A, N),
%%     rewrite((:-external_definition(Head, Head, TableName, Types))).
%% 
%% rewrite((:-external_predicate(A/N , B/N, TableName, Types))):- !,
%%     functor(Head, B, N),
%%     functor(Body, A, N),
%%     Head =.. [_|L],
%%     Body =.. [_|L],
%%     rewrite((:-external_definition(Head, Body, TableName, Types))).

rewrite(T):-
    format("~q.\n\n",[T]),
    myread(Term),
    rewrite(Term).
  
myread(X) :-
	read(X),
	debug(user_error,"Read: ~q\n",[X]).
	

% ----------------------------------------------------------------------------

declare_table(RelName,TableName,Types,ArgNames) :-
	length(Types,Arity),
	format("relation(~q,~q,~q).\n\n",[RelName,Arity,TableName]),
	declare_args(Types,ArgNames,1,TableName).

% ----------------------------------------------------------------------------

declare_args([],[],_,_) :-
	!,
	format("\n",[]).
declare_args([Type|Ts],[ArgName|As],N,TableName) :- 
	sqltype(Type),
	!,
	format("attribute(~q,~q,~q,~q).\n",[N,TableName,ArgName,Type]),
	N1 is N+1,
	declare_args(Ts,As,N1,TableName).
declare_args([T|_],[_|_],_,TableName) :- 
	!,
	error("illegal SQL type ~w in ~w",[T,TableName]).
declare_args(_,_,_,TableName) :- 
	error("arity mismatch in declaration ~w",[TableName]).

% ----------------------------------------------------------------------------

debug(X,Y,Z) :- format(X,Y,Z).
debug(_,_,_).
