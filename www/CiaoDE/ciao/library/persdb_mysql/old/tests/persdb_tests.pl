:- include(library('persdb_sql/persdb_sql')).
:- use_module(library(format)).

 :- multifile issue_debug_messages/1.
 :- data issue_debug_messages/1.
%%% issue_debug_messages('persdb').
%%% issue_debug_messages('persdbrt_sql').
%%% issue_debug_messages('persdbtr_sql').
 issue_debug_messages('db_client').
%%% issue_debug_messages('string2term').
%%% issue_debug_messages('pl2sql').

sql_persistent_location(radiowebdb,
        db('SQL Anywhere 5.0 Sample', dba, sql , 'r2d5.dia.fi.upm.es':2020)).

:- sql_persistent( product(integer,    integer,  string, string ),
               product(quantity,   id,       name,    size  ),
               radiowebdb).

main1 :-
        format("Printing table:\n",[]),
        product(Quantity,Id,Name,Size),
        format("Tuple: ~w \t ~w \t ~w \t ~w \t\n",[Quantity,Id,Name,Size]),
%%        product${quantity=>Quantity, name=>Name}, 
%%        format("Tuple: ~w \t ~w \t \n",[Quantity, Name]),
        fail.
main1 :-
        format("Done.\n",[]).


%% Main 2 issues a complex query inside a dbfindall:
main2 :-
	dbfindall(radiowebdb,
	           foo(Quantity, Id, Name, Size, Bar),
		   ( product(Quantity, Id, Name, Size),
                     I^N^S^(Bar is avg(Q,product(Q, I, N, S)))
		   ),
		   Results),
	format("Results = ~w~n",[Results]).

%% Main 3 - looking for products with same name but different Id
main3 :-
        format("Printing table:\n",[]),
%%	Name = 'Baseball Cap',
        dbcall( radiowebdb, (
	           product(_, Id1, Name, _), 
                   product(_, Id2, Name, _) 
                             )), 
	Id1 < Id2,
        format("Ids \t~w  and \t~w shared by ~w\n",[Id1, Id2, Name]),
        fail.
main3 :-
        format("Done.\n",[]).

%% Main 4 - same, but with findall. (DOES NOT WORK)
main4 :-
        dbfindall(radiowebdb,
	           ids_shared(Id1,Id2,Name),
	           ( product(_, Id1, Name, _), 
		     product(_, Id2, Name, _),
		     Id1 < Id2 ), 
		   Results), 
	format("Results = ~w~n",[Results]).


%% --------------------------------------------------------------------- %%
%% Third sample database : 'Literature'                                  %%
%% --------------------------------------------------------------------- %%

sql_persistent_location(literature_db,
	db('Literature','dba','sql','r2d5.dia.fi.upm.es':2020)).

%% Declare DYNAMICALLY authors/3 a persistent predicate,
%% storage in 'literature_db' 
main9:-
	make_sql_persistent(authors(string,string,integer),
 	      authors(firstName,lastName,id), %% 'id' is the primary key
 	      literature_db).

main10:- %% retracting a set of persistent facts
	dbretractall_fact(authors('Pedro',_X,_Y)).

main11:- %% assert a persistent fact
 	dbassertz_fact(authors('Fernando','Schwartz',26)).

main12:-
	dbfindall(literature_db,
	      authors(X,Y,Z),
	      Results)

%% retract a persistent fact
%% 	dbretract_fact(authors('Pedro','Calderon de la Barca',17)).
