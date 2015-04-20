:- include(library('persdb_sql/persdb_sql')).
:- use_module(library(format)).

%% --------------------------------------------------------------------- %%
%% Third sample database : 'LitAccess'                                   %%
%%  similar to Literature, used for tests                                %%
%%  (Sybase SQL Anywhere has some bugs, e.g.                             %%
%%	it returns 'Pedro Ca' instead 'Pedro Calderon de la Barca',...)  %%
%% --------------------------------------------------------------------- %%

:- sql_persistent(lit_authors(string,string,int),
 	      dba_authors(firstName,lastName,id), %% 'id' is the primary key
 	      lit_db).
sql_persistent_location(lit_db,db('litAccess','dba','sql','r2d5.dia.fi.upm.es':2020)).

main10:- %% Prints the contents of lit_authors
	print_predicate(lit_authors(_FirstName,_LastName,_Id)).

main11:- %% Issues a complex query inside a db_findall 
	dbfindall(lit_db,pred(First,Last,Id),(lit_authors(First,Last,Id),(Id<100))
		  ,Results),
	format("Results = ~w~n.",[Results]).

main12:- %% asserting several persistent facts
	dbassertz_fact(lit_authors('Pedro','Calderon de la Barca',2)),
	dbassertz_fact(lit_authors('Pedro','Calderon de la Barca',7)).

main13:- %% retracting a set of persistent facts
	dbretractall_fact(lit_authors('Pedro','Calderon de la Barca',_X)).

main14:- 
	dbretract_fact(lit_authors('Pedro','Calderon de la Barca',_X)).
%%    dbretract_fact is not still working in ODBC Access Driver. Permission to delete
%%    from a view is not given in retract_fact translated sentence, but in
%%    retractall_fact it is!!! (driver inconsistency).
