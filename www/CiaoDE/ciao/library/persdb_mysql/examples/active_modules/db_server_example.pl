:- module(db_server_example, 
	  [main/0, main0/0, main1/0, main2/0], 
	  [persdb_mysql,functions]).

:- use_module(library('persdb_mysql/mysql_client')).
%:- use_package(persdb_mysql).
:- use_module(library(format)).
:- use_module(library(lists)).

 %% :- multifile issue_debug_messages/1.
 %% :- data issue_debug_messages/1.
 %% issue_debug_messages('db_client').

main :- main2.

main0 :-
	mysql_connect(localhost:0,people,clip,'2by4LOG',DbConnection),
	mysql_get_tables(DbConnection, Tables),
	mysql_disconnect(DbConnection),
	format("Results: ~w \n",Tables),
	nl, display(Tables).
	
main1 :-
	sql_get_tables(db(people, clip, '2by4LOG', localhost:0),Tables),
	format("Results: ~w \n",Tables).

main2 :-
 	sql_table_types(db(people, clip, '2by4LOG', localhost:0),
	                people, TTypes),
	format("Results: ~w \n",TTypes).
