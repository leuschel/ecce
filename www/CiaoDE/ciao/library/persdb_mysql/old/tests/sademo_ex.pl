:- use_package(persdb_sql).
:- use_module(library(format)).

sql_persistent_location(sademo,
	 db('ASA 6.0 Sample','dba','sql','r2d5.dia.fi.upm.es':2020)).

main0:-
	sql_get_tables(sademo,TablesList),
	display(TablesList).

main1:-
	sql_table_types(sademo, 'Customer', AttList),
	display(AttList),
	sql_table_types(sademo, 'Employee', AttList2),
	display(AttList2).

:- sql_persistent(customer(integer,string,string,string,string,string,string,string,string),
	customer(id,fname,lname,address,city,state,zip,phone,company_name),
	sademo).

main2:-
	dbfindall(sademo,
	  customer(Id,Fname,Lname,Address,City,State,Zip,Phone,CompName),
	  customer(Id,Fname,Lname,Address,City,State,Zip,Phone,CompName),
	  List),
	display(List).
