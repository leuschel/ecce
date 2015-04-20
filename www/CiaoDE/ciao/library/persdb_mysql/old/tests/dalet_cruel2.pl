:- module(dalet_cruel2,
	  [dalet/1, dalet_table_info/3,
	   main1/0, main2/0, main4/0],
	   %% main3/5
	  [assertions, persdb_sql]).

:- use_module(library(lists)).
:- use_module(library('persdb_sql/sql2pltypes'),[sybase2sqltypes_list/2]).
:- use_module(library(write)).


:- multifile issue_debug_messages/1.
:- data issue_debug_messages/1.

%% issue_debug_messages(pl2sql).
issue_debug_messages(db_client).
issue_debug_messages(pl2sql).

%%%%
%%%
%%  DALET DATABASE INTERFACE
%


:- pred dalet(Goal)

   # "Calls the @var(Goal), which should be a table in the dalet
      database. If the table is not already declared persistent it is
      created on the fly.".

dalet(Goal) :-
	init_sql_persdb,
	functor(Goal, TableName, Arity),
	relation(TableName, Arity, _),
	!, display(1), call(Goal).
dalet(Goal) :-
	functor(Goal, TableName, Arity),
	dalet_table_info(TableName, AttrNameLs, AttrTypeLs),
	length(AttrNameLs, Arity),
	sybase2sqltypes_list(AttrTypeLs,AttrCIAOTypesLs),
	dalet_make_persistent(TableName, AttrNameLs, AttrCIAOTypesLs),
	!, display(2), call(Goal).


:- pred dalet_table_info(?(TableName), -AttrNameLs, -AttrTypeLs)

   # "Returns the @var(AttrNameLs) and @var(AttrTypeLs) for the attributes
      of a table with @var(TableName). If the @var(TableName) is a
      variable, the predicate returns information of all tables upon
      backtracking.".

dalet_table_info(TableName, AttrNameLs, AttrTypeLs) :-
	sql_get_tables(daletdb, TableLs),
	!, member(TableName, TableLs),
	sql_table_types(daletdb, TableName, AttrLs),
	split_attrlist(AttrLs, AttrNameLs, AttrTypeLs).


:- pred split_attrlist(AttrLs, AttrNameLs, AttrTypeLs)

   # "Splits an @var(AttrLs) as returned from sql_table_types/3 into two
      lists; @var(AttrNameLs) and @var(AttrTypeLs). ".

split_attrlist([], [], []).
split_attrlist([[Name,Type]|Ls], [Name|NameLs], [Type|TypeLs]) :-
	split_attrlist(Ls, NameLs, TypeLs).


:- pred dalet_make_persistent(+TableName, +AttrNameLs, +AttrTypeLs)

   # "Creates a persistent predicate for the @var(TableName) with
      @var(AttrNameLs) and @var(AttrTypeLs). The predicate will be
      connected to the dalet database. Note that no existence checks are
      made.".

dalet_make_persistent(TableName, AttrNameLs, AttrTypeLs) :-
	PredTerm =.. [TableName | AttrTypeLs],
	NameTerm =.. [TableName | AttrNameLs],
	make_sql_persistent(PredTerm, NameTerm, daletdb), !.



:- pred dalet_make_tables

   # "Creates persistent predicates for all tables in the dalet database.".

dalet_make_tables :-
	dalet_table_info(TableName, AttrNameLs, AttrSybaseTypeLs),
	sybase2sqltypes_list(AttrSybaseTypeLs,AttrTypeLs),
	dalet_make_persistent(TableName, AttrNameLs, AttrTypeLs),
	fail.
dalet_make_tables.



%%%%
%%%
%%  DALET DATABASE CONNECTION
%

sql_persistent_location(daletdb,
	                db('DaletDemo', 'dalet_admin', 'dalet_admin',
			   'r2d5.dia.fi.upm.es':2021)).



%%%%
%%%
%%  PREDEFINED PREDICATES
%

% Simple long field table
:- sql_persistent(
	simple_long_field(string,'long varchar'),
	'Disco_Artist'('lname','text'),
	daletdb).

% Simple artist table
%:- sql_persistent(
%	simple_artist(integer,char,char,char,char),
%	'Disco_Artist'('Disco_Artist','fname','lname','birth_place','email'),
%	daletdb).



%%%%
%%%
%%  MAIN ROUTINES
%


%
% Create all database tables, with their respective attributes.
%
% NOTE: Should not give errors for database types.
%
main1 :-
	dalet_make_tables.


%
% Tests accessing the whole Disco_Artist table.
%
% NOTE: Should not cause errors for the long 'text' field.
%
main2 :-
	simple_long_field("Lennon",_X).



%
% Tests the reordering and skipping of certain fields in the database table.
%
% NOTE: The first field (A) should not be 'text', since the predicate uses
%       simple_artist.
% NOTE: Should not unify the variables for the fields with NULL values.
%
%% main3(A,B,C,D,E) :-
%%	simple_artist(A,B,C,D,E).


%
% Tests the call via the dalet/1 predicate.
%
% NOTE: Should not produce a warning, as the predicate actually exists.
%
main4 :-
	dalet(artist_type(_,_)).
