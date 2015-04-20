:- multifile([relation/3,attribute/4]).
:- data([relation/3,attribute/4]).

:-use_module(library('persdb_sql/misc/pl2sql_orig')).
:-include(library('persdb_sql/misc/tables')).

main((Head :- Body),T) :-
	pl2sql(Head,Body,SQLStringTerm),
	(  T = s 
	-> queries_atom(SQLStringTerm,Atom),
	   write(Atom)
	;  printqueries(SQLStringTerm) ),
	!.
