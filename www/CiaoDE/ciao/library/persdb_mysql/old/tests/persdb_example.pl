:- include(library('persdb_sql/persdb_sql')).
:- use_module(library(format)).

%% ------------------------------------------------- %%
%% First sample database : 'SQL Anywhere 5.0 Sample' %%
%% ------------------------------------------------- %%

%% Declare product/4 a persistent predicate, storage in 'radiowebdb':
:- sql_persistent(product( int,    int, string, string ),
              product( quantity,   id,      name,   size   ),
              sampledb).
sql_persistent_location(sampledb, %% The 'sampledb' descriptor:
         db('SQL Anywhere 5.0 Sample', 'dba', 'sql','r2d5.dia.fi.upm.es':2020)).

main0 :-
%% Prints the contents of the relation 'product/4' by backtracking over it:
         format("Printing table:\n",[]),
         product(Quantity, Id, Name, Size), 
         format("Tuple: ~w \t ~w \t ~w \t ~w \n",[Quantity, Id, Name, Size]),
         fail.
main0 :-
         format("Done.\n",[]).

%% Generalizing tables printing :
print_predicate(Pred):- %% Prints the contents of Pred by backtracking over it
        format("Printing relation:\n",[]),
	Pred, %% predicate call
	Pred=..[_|Args],
        format("\t Tuple: ~w \n",[Args]),
        fail.
print_predicate(_Pred):-
	format("Done.~n",[]).

main1 :- %% similar to main0:
	 %% Prints the contents of the relation 'product/4' by backtracking over it:
	print_predicate(product(_Quantity,_Id,_Name,_Size)).

main2 :- %% Issues a complex query inside a db_findall:
         dbfindall(sampledb,
                   foo(Quantity, Id, Name, Size, Bar),
                   ( product(Quantity, Id, Name, Size),
                     I^N^S^(Bar is avg(Q,product(Q, I, N, S)))
                   ),
                   Results),
         format("Results = ~w~n",[Results]).
main21 :- %% Using db_call
         dbcall(sampledb,
             (product(Quantity, Id, Name, Size),product(Quantity, Id1, _Name1, _Size1),Id1>Id)),
%%       product(Quantity, Id, Name, Size),
%% 	 product(Quantity, Id1, Name1, Size1),
%% 	 Id1>Id,
         format("Tuple: ~w \t ~w \t ~w \t ~w \n",[Quantity, Id, Name, Size]).


%% ------------------------------------------------- %%
%% Second sample database : 'Literature'             %%
%% ------------------------------------------------- %%

:- sql_persistent(authors(string,string,int),
 	      authors(firstName,lastName,id), %% 'id' is the primary key
 	      literature_db).
sql_persistent_location(literature_db,
	db('Literature','dba','sql','r2d5.dia.fi.upm.es':2020)).

main3:- %% Prints the contents of authors
         print_predicate(authors(_FirstName,_LastName,_Id)) .

main4:- %% assert a persistent fact
 	dbassertz_fact(authors('Pedro','Calderon de la Barca',17)).

main6:- %% retract a persistent fact
 	dbretract_fact(authors('Pedro','Calderon de la Barca',17)).

:- data nonpersistentfact/2.
main5:- %% asserting non-persistent facts
	assertz_fact(nonpersistentfact(6,_X)),
	assertz_fact(nonpersistentfact(3,18)),
	assertz_fact(nonpersistentfact(3,19)).

main7:- %% retracting a non-persistent fact (or all, by backtracking over it)
	dbretract_fact(nonpersistentfact(3,_X)).

main8:- %% retracting a set of non-persistent facts
	dbretractall_fact(nonpersistentfact(3,_X)).

main9:- %% checking if a persistent fact is a current fact
	dbcurrent_fact(authors(_X,_Y,_Z)).

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
