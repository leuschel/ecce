:- use_package(persdb_sql).
:- use_module(library(format)).

%:- multifile issue_debug_messages/1.
%:- data issue_debug_messages/1.

%issue_debug_messages(db_client).


%% ------------------------------------------------- %%
%% First sample database : 'SQL Anywhere 5.0 Sample' %%
%% ------------------------------------------------- %%

%% Declare STATICALLY product/4 a persistent predicate, storage in 'sampledb'
:- sql_persistent(product( int,    int, string, string ),
                  product( quantity,   id,      name,   size   ),
                  sampledb).

sql_persistent_location(sampledb, db('SQL Anywhere 5.0 Sample', 'dba', 'sql','r2d5.dia.fi.upm.es':2020)).

sql_persistent_location(literature_db, db('Literature', 'dba', 'sql','r2d5.dia.fi.upm.es':2020)).

data sql_persistent_location/2.
main0:-
	sql_get_tables(literature_db,AccessTablesList),
	AccessTablesList=[T1|_],
	sql_table_types(literature_db, T1, AttL),
	format("SampleAccess tables list is ~w~n",AccessTablesList),
	format("SampleAccess table atts. is ~w~n",AttL),
	obtain_ids_types(AttL,As,Ts),
	SQLDef=..[T1|As],
	PrologDef=..[T1|Ts],
	make_sql_persistent(PrologDef, SQLDef, literature_db).
obtain_ids_types([],[],[]).
obtain_ids_types([[Id,Type]|Rest],[Id|Ids],[Type|Types]):-
	obtain_ids_types(Rest, Ids, Types).


main1 :-
%% Prints the contents of the relation 'product/4' by backtracking over it:
         format("Printing table:\n",[]),
         product(Quantity, Id, Name, Size), 
         format("Tuple: ~w \t ~w \t ~w \t ~w \n",[Quantity, Id, Name, Size]),
         fail.
main1 :-
         format("Done.\n",[]).


main2 :- %% Issues a complex query inside a db_findall:
         dbfindall(sampledb,
                   foo(Quantity, Id, Name, Size, Bar),
                   ( product(Quantity, Id, Name, Size),
                     I^N^S^(Bar is avg(Q,product(Q, I, N, S)))
                   ),
                   Results),
         format("Results = ~w~n",[Results]).

main3 :- %% Using db_call
         dbcall(sampledb,
                ( product(Quantity, Id, Name, Size),
                  product(Quantity, Id1, _Name1, _Size1),
                  Id1>Id ) ),
         format("Tuple: ~w \t ~w \t ~w \t ~w \n",[Quantity, Id, Name, Size]).


%% --------------------------------------------------------------------- %%
%% Second sample database : 'LitAccess'                                  %%
%% --------------------------------------------------------------------- %%


sql_persistent_location(lit_db,
     db('litAccess','dba','sql','r2d5.dia.fi.upm.es':2020)).

%% Declare DYNAMICALLY lit_authors/3 a persistent predicate, 
%% storage in 'literature_db' 
main4:- %% Declares dynamically a persistent predicate
	make_sql_persistent(lit_authors(string,string,int),
 	      dba_authors(firstName,lastName,id), 
 	      lit_db).

%%%%%% TO REVIEW: 
main5:- %% Prints an element of lit_authors
	lit_authors(FirstName,LastName,Id),
	format("Tuple: ~w \t ~w \t ~w \n",[FirstName, LastName, Id]).

main6:- %% Issues a complex query inside a db_findall 
	dbfindall(lit_db,
                  pred(_Id),
                  (lit_authors(_First,_Last,_Id)),
		  Results),
	format("Results = ~w~n.",[Results]).

main7:- %% asserting several persistent facts
	dbassertz_fact(lit_authors('Pedro','Calderon de la Barca',2)),
	dbassertz_fact(lit_authors('Pedro','Calderon de la Barca',7)).

main8:- %% checking if a persistent fact is a current fact
	dbcurrent_fact(lit_authors(_X,_Y,_Z)).


%% --------------------------------------------------------------------- %%
%% Third sample database : 'Literature'                                  %%
%% --------------------------------------------------------------------- %%

sql_persistent_location(literature_db,
	db('Literature','dba','sql','r2d5.dia.fi.upm.es':2020)).

%% Declare DYNAMICALLY authors/3 a persistent predicate,
%% storage in 'literature_db' 
main9:-
	make_sql_persistent(authors(string,string,int),
 	      authors(firstName,lastName,id), %% 'id' is the primary key
 	      literature_db).

main10:- %% retracting a set of persistent facts
	dbretractall_fact(authors('Pedro',_X,_Y)).

main11:- %% assert a persistent fact
 	dbassertz_fact(authors('Pedro','Calderon de la Barca',17)).

%% retract a persistent fact
%% 	dbretract_fact(authors('Pedro','Calderon de la Barca',17)).
