:- include(library('persdb_sql/persdb_sql')).
:- use_module(library(format)).

%% ------------------------------------------------- %%
%% First sample database : 'SA 6.0 Sample' %%
%% ------------------------------------------------- %%

%% Declare product/4 a persistent predicate, storage in 'radiowebdb':
:- sql_persistent(product( int,    int, string, string ),
              product( quantity,   id,      name,   size   ),
              sampledb).
sql_persistent_location(sampledb, %% The 'sampledb' descriptor:
         db('ASA 6.0 Sample','dba', 'sql','r2d5.dia.fi.upm.es':2020)).

main0 :-
%% Prints the contents of the relation 'product/4' by backtracking over it:
         format("Printing table:\n",[]),
         product(Quantity, Id, Name, Size), 
         format("Tuple: ~w \t ~w \t ~w \t ~w \n",
	        [Quantity, Id, Name, Size]),
         fail.
main0 :-
         format("Done.\n",[]).

%% Generalizing table printing:
%% Prints the contents of Pred by backtracking over it
print_predicate(Pred):- 
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
             (product(Quantity, Id, Name, Size),
              product(Quantity, Id1, _Name1, _Size1),Id1>Id)),
%%       product(Quantity, Id, Name, Size),
%% 	 product(Quantity, Id1, Name1, Size1),
%% 	 Id1>Id,
         format("Tuple: ~w \t ~w \t ~w \t ~w \n",
                [Quantity, Id, Name, Size]).

main22 :-  % get the set of tables from a database
	sql_get_tables(sampledb,TablesList),
	display(TablesList).

main23 :- % get table attributes and its types 
	sql_table_types(sampledb, 'Customer', AttList),
	display(AttList).
	
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


main9:- %% checking if a persistent fact is a current fact
	dbcurrent_fact(authors(_X,_Y,_Z)).
