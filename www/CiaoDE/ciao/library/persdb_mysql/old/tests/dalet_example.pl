%%%%%%%%%%%% LIBRARIES :

:- syntax(persdb_sql).
:- use_module(library(format)).

%%%%%%%%%%%% EXTERNAL TABLES' DEFINITION:

:- sql_persistent(titles(integer,string,string,string,integer,integer),
	titles(title_id,title,interpret,author,duration,title_type_id),
	dalet_db).
:- sql_persistent(title_types(integer,string),
	title_types(title_type_id,name),
	dalet_db).

sql_persistent_location(dalet_db,
	db('DaletDemo','dalet_admin','dalet_admin','r2d5.dia.fi.upm.es':2020)).

%%%%%%%%%%%% EXAMPLE of USE (EXTERNAL PERSISTENT PREDICATES)

main1:-
%% Prints all the audio&text records interpreted by Dylan or Reed with a
%% duration longer than 30000
	format("Printing titles, interprets and durations: \n\n",[]),
	title_types(_Type,'AUDIO_AND_TEXT'),
	( Author='Lou Reed' ; Author='Bob Dylan'),
	titles(_Id,Title,Interpret,Author,Duration,_Type),
	Duration>30000,
	format("\t~w, ~w, ~w\n",[Title,Interpret,Duration]),
	fail.
main1:-
	format("\nDone. \n",[]).


main2:-
%% Displays a list containing all the audio&text records composed by Dylan
%% or interpreted by U2
	dbfindall(dalet_db,
                  song(Tit,Duration),
		  (
                   titles(_Id,Tit,Interpret,Author,Duration,_Type),
		   title_types(_Type,'AUDIO_AND_TEXT'),
		   ( Author='Bob Dylan' ; Interpret='U2')
                  )
                  ,List),
        display(List).



%% Selected facts can be deleted from the external database using
%% dbretractall_fact/1, provided that integrity constraints are respected.

deletion :-
        dbretract_fact(titles(_,'Nashville Skyline',_,_,_,_)).

%% External predicates can also be asserted to a database using
%% dbassertz_fact/1 predicate (but of course, unique keys cannot be
%% inserted two times):

insertion:-
	dbassertz_fact(
          titles(507,'Nashville Skyline','Bob Dylan','Bob Dylan',32342,2)
        ).
