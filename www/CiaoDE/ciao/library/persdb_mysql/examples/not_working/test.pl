%:- module(_, _, [persdb_mysql, functions]).

:- use_package([persdb_mysql, functions]).
:- use_module(library(write)).
:- use_module(library(format)).

sql_persistent_location(people, db(people, clip, '2by4LOG', localhost:0)).

:- sql_persistent(people(string, string, int), people(name, sex, age), people).

% Low level MySQL interface.

:- use_module(library('persdb_mysql/mysql_client')).

% Create a database and a table of people.

create_people_db :-
	mysql_connect(localhost:0, '', clip, '2by4LOG', DbConnection),
	write(~mysql_query(DbConnection, "drop database if exists people")), nl,
	write(~mysql_query(DbConnection, "create database people")), nl,
	write(~mysql_query(DbConnection, "use people")), nl,
	write(~mysql_query(DbConnection, "create table people(name char(16) not null, sex text, age int, primary key(name))")), nl,
	mysql_disconnect(DbConnection).

% Inserts people into the 'people' table.

male(john, 15).
male(peter, 24).
male(ralph, 24).
male(bart, 50).
female(kirsten, 24).
female(mary, 17).
female(jona, 12).
female(maija, 34).

insert_people :-
	(male(N, A), dbassertz_fact(people(N, male, A)), fail ; true),
	(female(N, A), dbassertz_fact(people(N, female, A)), fail ; true).

% Removes people from the 'people' table.

remove_people :-
        dbretractall_fact(people(_, _, _)).

remove_people2 :-
        dbretract_fact(people(N, S, A)),
        display(dbretract_fact(people(N, S, A))), nl,
	fail.
remove_people2 :-
	display(end), nl.

test :-
	people(Name, Sex, Age),
	display(people(Name, Sex, Age)),
	nl,
	fail.
test :-
	display('DONE'), nl.
	
main :-
	test.


test_tables :-
	sql_get_tables(db(people, clip, '2by4LOG', 'clip.dia.fi.upm.es':0),Tables),
	format("Results: ~w \n", [Tables]).
