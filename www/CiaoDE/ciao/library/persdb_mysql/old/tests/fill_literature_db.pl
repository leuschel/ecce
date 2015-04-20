:- include(library('persdb_sql/persdb_sql')).
:- use_module(library(format)).


 :- multifile issue_debug_messages/1.
 :- data issue_debug_messages/1.
 issue_debug_messages('persdb').
 issue_debug_messages('persdbrt_sql').
 issue_debug_messages('persdbtr_sql').
 issue_debug_messages('db_client').
 issue_debug_messages('string2term').
 issue_debug_messages('pl2sql').



sql_persistent_location(litDB,
	db('Literature',dba,sql,'r2d5.dia.fi.upm.es':2020)).
:- sql_persistent(authors(int,string,string),
	          authors(id,lastname,firstname),
		  litDB).
:- sql_persistent(authors(string,string),
	          authors(firstname,lastname),
		  litDB).

main1:-
	dbassertz_fact(authors(1,'Garcia Marquez','Gabriel')).

main2(Id,LastName,FirstName):-
	authors(Id,LastName,FirstName).

gabrielAuthors(Name):-
	authors('Gabriel',LastName),
        atom_concat('Gabriel ',LastName,NameAt),
	atom_codes(NameAt,Name).

printGabrielAuthors:-
	gabrielAuthors(Name),
	format("\tAuthor: ~s\n",[Name]),
	fail.
printGabrielAuthors:-
	format("Done \n",[]).

print_sql_persistent:-
	'$is_sql_persistent'(X,Y,Z),
	format("~w, ~w, ~w \n",[X,Y,Z]),
	fail.
print_sql_persistent:-
	format("Done \n",[]).


ask:-
	write('Stop (Y/N)?'),
	read(Car),
	((Car == 'Y') ;(Car == 'y')),
	!,
        nl,write('Read stopped'),nl.
         
ask:-
	insertData.
	
insertData:-
	nl,
	write('Please, insert data using an atom''s format'), nl,
	write('FirstName: '),
	read(FirstName), nl,
	write('LastName: '),
	read(LastName), nl,
	write('Id: '),
	read(Id),nl,
	dbassertz_fact(authors(Id,LastName,FirstName)),
	ask,
	!,
	write('Facts were asserted'),nl.

insertData:-
	write('The full data couldn''t be asserted').
