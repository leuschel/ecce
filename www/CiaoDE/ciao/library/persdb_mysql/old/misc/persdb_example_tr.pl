:- multifile(relation/3).

:- multifile(attribute/4).

:-include(library('persdb_sql/persdb')).

:-multifile persistent_location/2.

:-data persistent_location/2.

persistent_location(radiowebdb,db('SQL Anywhere 5.0 Sample','','r2d5.dia.fi.upm.es':2020)).

relation(quantity,4,'PRODUCT').

attribute(1,'PRODUCT','QUANTITY',integer).
attribute(2,'PRODUCT','ID',integer).
attribute(3,'PRODUCT','NAME',string).
attribute(4,'PRODUCT','SIZE',real).

quantity(_2759,_2760,_2761,_2762) :- 
	db_call(quantity(_2759,_2760,_2761,_2762),radiowebdb).
