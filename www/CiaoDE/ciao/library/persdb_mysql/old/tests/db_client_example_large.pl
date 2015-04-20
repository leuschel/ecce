%%  WARNING: Errors handling has been changed in db_client predicates. Next
%%  predicates work properly if no unexpected event occurs. However, to handle 
%%  errors, arguments have to be checked after the respective predicate is called 
%%  (see db_client definitions).

:- use_module(library('persdb_sql/db_client')).
:- use_module(library(format)).

 :- multifile issue_debug_messages/1.
 :- data issue_debug_messages/1.
%%% issue_debug_messages('persdb').
 issue_debug_messages('persdbrt_sql').
%%% issue_debug_messages('persdbtr_sql').
 issue_debug_messages('db_client').
%%% issue_debug_messages('string2term').
%%% issue_debug_messages('pl2sql').

main0 :- %% accessing the whole table in one go
	odbc_connect('r2d5.dia.fi.upm.es':2020,Stream), 
	db_login(Stream,'DaletDemo',dalet_admin,dalet_admin,Conn),
	db_eval_sql(Conn,"SELECT * from titles",Term), 
        write(Term), nl,
	db_logoff(Conn),
	odbc_disconnect(Stream).

main1 :- %% accessing the whole table in one go
	odbc_connect('r2d5.dia.fi.upm.es':2020,Stream), 
	db_login(Stream,'SQL Anywhere 5.0 Sample',dba,sql,Conn),
	db_eval_sql(Conn,"SELECT fname,lname,address from ""DBA"".customer 
		   WHERE ((Id>100) AND (Id<105))",Term), 
        write(Term), nl,
	db_eval_sql(Conn,"SELECT id,name,description,color from 
                    ""DBA"".product WHERE size='One size fits all' 
                    OR size='Large'",Term2), 
        write(Term2), nl,
	db_logoff(Conn),
	odbc_disconnect(Stream).

main2:- %% accessing tuples one by one
	odbc_connect('r2d5.dia.fi.upm.es':2020,Stream),
	db_login(Stream,'SQL Anywhere 5.0 Sample',dba,sql,Conn),
	db_stmt_handle(Conn,"SELECT fname,lname,address from ""DBA"".customer 
                     WHERE ((Id>100) AND (Id<105))",QueryConn),
	db_one_tuple(QueryConn,Answer),
	format("First Tuple: ~w \n",Answer),
	db_one_tuple(QueryConn,Answer2),
	format("Second Tuple: ~w \n",Answer2),
	db_logoff(Conn),
	odbc_disconnect(Stream).
	
main3:- %% bringing tuples (one to one) from Literature Database
	odbc_connect('r2d5.dia.fi.upm.es':2020,Stream),
	db_login(Stream,'Literature',dba,sql,Conn),
	db_stmt_handle(Conn,"SELECT * FROM AUTHORS ORDER BY ID",QueryConn),
	print_all_one_by_one(QueryConn),
	db_logoff(Conn),
	odbc_disconnect(Stream).
%% LOOK : INFINITE LOOP !!! 

%% NOTE: db_one_tuple(QueryConn,[]) if all the tuples have been fetched

print_all_one_by_one(QueryConn):-
	db_one_tuple(QueryConn,Answer), %% with [] unification doesn't work 
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
	db_stmt_handle(Conn,"INSERT INTO AUTHORS values ('Martin Gaite','Carmen',10,'1910-7-10 00:00:00:000')",_QueryConn),
	db_logoff(Conn),
	odbc_disconnect(Stream).
%% BUG: reported dates (e.g. main2) do not match those specified in 
%% the INSERT sentence. This is due to a bug in the evaluation copy of the
%% SQL Anywhere ODBC driver.

main5:- %% creating a view
	odbc_connect('r2d5.dia.fi.upm.es':2020,Stream),
	db_login(Stream,'Literature',dba,sql,Conn),
	db_stmt_handle(Conn,"CREATE VIEW AntoniosDateOfBirth AS SELECT LastName,Date_of_birth FROM AUTHORS where FirstName='Antonio';",_QueryConn),
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
