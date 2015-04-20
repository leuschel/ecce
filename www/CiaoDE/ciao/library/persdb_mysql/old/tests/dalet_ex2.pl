%%%%%%%%%%%% LIBRARIES :

:- include(library('persdb_sql/persdb_sql')).
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

main:-
	dbfindall(dalet_db,dylan_sting_titles(Tit,Duration),
		  (titles(_Id,Tit,Interpret,Author,Duration,Type),
		   title_types(Type,'AUDIO_AND_TEXT'),
		   ((Author='Bob Dylan');(Interpret='Sting')))
                  ,List),
        write(List).
