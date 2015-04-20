:- module(sqlexpand,[rewrite_program/1,rewrite/1]).

:- include(library(assertions)).
:- use_module(library(strings)).
:- use_module(library(dec10_io)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library('persdb_new/pl2sql')).

rewrite_program(InNoPL):-
    atom_concat(InNoPL,'.pl',In),
    atom_concat(InNoPL,'_tr.pl',Out),
    see(In),
    tell(Out),
    write_headings,
    read(Term),
    rewrite(Term),
    told,
    seen.
    
write_headings:-
    format(":- include(library('persdb_new/sqlitf')).\n\n",[]),
    format(":- multifile(relation/3).\n\n",[]),
    format(":- multifile(attribute/4).\n\n",[]).

rewrite(end_of_file):- 
    !.

rewrite((:- persistent(PrologDef,SQLDef,_DBId))) :- !,
    PrologDef =.. [PrologName | Types],
    SQLDef    =.. [TableName  | ArgNames],
    declare_table(PrologName,TableName, Types, ArgNames),
    functor(PrologDef,PrologName,Arity),
    functor(Consequence,PrologName,Arity),
    format("~q :- \n\tcopy_term(~q,NConseq),\n", [Consequence,Consequence]),
    format("\tpl2sqlstring(NConseq,(NConseq),SQLStringQuery),\n", []),
    format("\tsql_fast(SQLStringQuery,ResultsPredicate),\n",[]),
    format("\tResultsPredicate = table(_,ResultsList),\n",[]),
    Consequence=.. [_|OneResult],
    format("\tmember(~w,ResultsList).\n\n",[OneResult]),
    read(Term),
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
    writeq(T), write('.'),
    nl,
    read(Term),
    rewrite(Term).
  
declare_table(RelName,TableName,Types,ArgNames) :-
	length(Types,Arity),
	format("relation(~q,~q,~q).\n\n",[RelName,Arity,TableName]),
	declare_args(Types,ArgNames,1,TableName).

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
	format(user_error,"[WARNING: illegal SQL type ~w in ~w]~n",
	                   [T,TableName]).
declare_args(_,_,_,TableName) :- 
	format(user_error,"[WARNING: arity mismatch in declaration ~w]~n",
	                  [TableName]).
