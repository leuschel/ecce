:- use_module(library('persdb_sql/db_client')).
:- use_module(library(format)).
:- use_module(library(lists)).

:- multifile issue_debug_messages/1.
:- data issue_debug_messages/1.
issue_debug_messages('db_client').

main0:- %% getting the tables existing in a database
	odbc_connect('r2d5.dia.fi.upm.es':2020,Stream),
	db_login(Stream,'DaletDemo',dalet_admin,dalet_admin,
%%	db_login(Stream,'ASA 6.0 Sample',dba,sql,
	         dbconnection(Stream,DbHandle)),
%%	db_get_tables(dbconnection(Stream,DbHandle),TablesList),
	db_table_types(dbconnection(Stream,DbHandle),'Titles',AttList),
	db_logoff(dbconnection(Stream,DbHandle)),
	odbc_disconnect(Stream),
	format("Results: ~w \n",AttList).


main1 :- %% accessing the whole table in one go
	odbc_connect('r2d5.dia.fi.upm.es':2020,Stream), 
	db_login(Stream,'ASA 6.0 Sample',dba,sql,Conn),
	db_eval_sql(Conn,"SELECT fname,lname,address from ""DBA"".customer 
		   WHERE ((Id>100) AND (Id<105))",Term), 
        write(Term), nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,
	functor(Term,X,_Y),
	write(X),nl,
	Term=..[_|L],
	write('That is L'),nl,	
	write(L),nl,L=[M],length(M,N),write(N),
%	db_eval_sql(Conn,"SELECT id,name,description,color from 
%                    ""DBA"".product WHERE size='One size fits all' 
%                    OR size='Large'",Term2), 
%        write(Term2), nl,
	db_logoff(Conn),
	odbc_disconnect(Stream).

main2:- %% accessing tuples one by one
	odbc_connect('r2d5.dia.fi.upm.es':2020,Stream),
	db_login(Stream,'ASA 6.0 Sample',dba,sql,Conn),
	db_stmt_handle(Conn,
	      "SELECT fname,lname,address from ""DBA"".customer 
               WHERE ((Id>100) AND (Id<105))",QueryConn),
	db_one_tuple(QueryConn,Answer),
	format("First Tuple: ~w \n",Answer),
	db_one_tuple(QueryConn,Answer2),
	format("Second Tuple: ~w \n",Answer2),
	db_logoff(Conn),
	odbc_disconnect(Stream).

main22 :-
	odbc_connect('r2d5.dia.fi.upm.es':2020,Stream),
	db_login(Stream,'Literature',dba,sql,Conn),
	db_stmt_handle(Conn,
	      "SELECT fname,lname,address from ""DBA"".customer 
               WHERE ((Id>100) AND (Id<105))",QueryConn),
	db_one_tuple(QueryConn,Answer),
	format("First Tuple: ~w \n",Answer),
	db_one_tuple(QueryConn,Answer2),
	format("Second Tuple: ~w \n",Answer2),
	db_logoff(Conn),
	odbc_disconnect(Stream).

	
main3:- %% bringing tuples (one to one) from Literature Database
	%% TO SEE : check it
	odbc_connect('r2d5.dia.fi.upm.es':2020,Stream),
	db_login(Stream,'Literature',dba,sql,Conn),
	db_stmt_handle(Conn,"SELECT * FROM AUTHORS ORDER BY ID",QueryConn),
	print_all_one_by_one(QueryConn),
	db_logoff(Conn),
	odbc_disconnect(Stream).

print_all_one_by_one(QueryConn):-
	db_one_tuple(QueryConn,Answer), 
	Answer\=[],
	!,
	format(" ~w \n",Answer),
	print_all_one_by_one(QueryConn).
print_all_one_by_one(_QueryConn):-
	write(' Finished fetching query statement.'),
	nl.

main4:- %% inserting a tuple into the Literature database
	odbc_connect('r2d5.dia.fi.upm.es':2020,Stream),
	db_login(Stream,'Literature',dba,sql,Conn),
	db_stmt_handle(Conn, 
               "INSERT INTO AUTHORS values ('Martin Gaite','Carmen',10)",_QueryConn),
	db_logoff(Conn),
	odbc_disconnect(Stream).
	
main5:- %% creating a view
	odbc_connect('r2d5.dia.fi.upm.es':2020,Stream),
	db_login(Stream,'Literature',dba,sql,Conn),
	db_stmt_handle(Conn,
               "CREATE VIEW AntoniosDateOfBirth AS SELECT 
                LastName,Date_of_birth FROM 
                AUTHORS where FirstName='Antonio';",_QueryConn),
	db_logoff(Conn),
	odbc_disconnect(Stream).	

main6:- %% printing the view
	odbc_connect('r2d5.dia.fi.upm.es':2020,Stream),
	db_login(Stream,'Literature',dba,sql,Conn),
	db_eval_sql(Conn,"select * from AntoniosDateOfBirth",Term),
	write(Term),
	db_logoff(Conn),
	odbc_disconnect(Stream).

main7:- %% dropping a view
	odbc_connect('r2d5.dia.fi.upm.es':2020,Stream),
	db_login(Stream,'Literature',dba,sql,Conn),
	db_eval_sql(Conn,"DROP VIEW AntoniosDateOfBirth",Term),
	write(Term),
	db_logoff(Conn),
	odbc_disconnect(Stream).
