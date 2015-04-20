:- module(_,
        [
	    init_sql_persdb/0,
%% Was just exported to document declaration (for now done with doinclude).
%	    sql_persistent/3,
%	    dbname/1,
%	    user/1,
%	    passwd/1,
%	    socketname/1,
 	    dbassertz_fact/1,
 	    dbretract_fact/1,
 	    dbcurrent_fact/1,
	    dbretractall_fact/1,
 	    make_sql_persistent/3,
	    dbfindall/4,
%	    projterm/1,
%	    querybody/1,
	    dbcall/2, %% Internal predicate
	    db_call_db_atomic_goal/2,
	    sql_query/3,
	    sql_get_tables/2,
	    sql_table_types/3
        ],[assertions, regtypes, basicmodes]).

:- reexport(library('persdb_sql/db_client'),
	            [socketname/1,dbname/1,user/1,passwd/1]).

:- reexport(library('persdb_sql/pl2sql_sql'),
	            [projterm/1,querybody/1]).

:- reexport(library('persdb_sql_common/sqltypes'),
	            [sqltype/1]).

%% sql_query_one_tuple_more/2 internal predicates

:- multifile sql_persistent_location/2.
:- data sql_persistent_location/2.

:- pred sql_persistent_location(Keyword,Location) ::  persLocId * persLocation

# "@var{Keyword} is an identifier for the persistent data location
  @var{Location}.".

:- regtype persLocation/1.

persLocation(db(Name, User, Password, Machine:Port)) :-
	atm(Name),
	atm(User),
	atm(Password),
	atm(Machine),
	int(Port).

:- multifile '$is_sql_persistent'/3.

:- comment(hide,'$is_sql_persistent'/3).

:- data sql_persistent/2.


:- use_module(library(dynamic)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(terms_vars), [varset/2]).
:- use_module(library(messages),[error_message/2,debug_message/2]).
:- use_module(library(lists),[length/2,append/3]).
%% list/1,list/2,
:- use_module(library(aggregates), [findall/3]).
:- use_module(engine(internals),[term_to_meta/2,module_concat/3]).

:- use_module(library('persdb_sql/db_client'),
	    [odbc_connect/2, db_login/5, dbconnection/1, db_eval_sql/3,
	    answertableterm/1, tuple/1, db_stmt_handle/3, dbqueryconnection/1,
	    db_one_tuple/2, answertupleterm/1, db_get_tables/2,
	    db_table_types/3, db_logoff/1, odbc_disconnect/1,
	    match_string/3]).

:- use_module(library('persdb_sql/pl2sql_sql'),
	[pl2sqlstring/3,sqlstring/1
	%% ,sqltype/1
	]).
:- use_module(library('persdb_sql_common/sqltypes'),
	[accepted_type/2
	%% ,sqltype/1
	]).
:- use_module(library('persdb_sql_common/pl2sqlinsert'),
	[pl2sqlInsert/2]).

:- comment(bug,"At least in the shell, reloading a file after changing
   the definition of a persistent predicate does not eliminate the old
   definition...").

:- comment(bug,"Functionality missing: some questions need to be debugged.").

:- comment(bug,"Warning: still using kludgey string2term and still
   using some non-uniquified temp files.").

:- comment(bug,"Needs to be unified with the file-based library.").

%% ---------------------------------------------------------------------------
%% Intro
%% ---------------------------------------------------------------------------

:- comment(doinclude, projterm/1).

:- comment(doinclude, querybody/1).


:- comment(usage, "Typically, this library is used including the
   'persdb_sql' package into the package list of the module, or using the
   @decl{use_package/1} declaration:
@begin{description}
@item{In a module:}
@begin{verbatim}
	:- module(bar, [main/1], [persdb_sql]).
@end{verbatim}
        or
@begin{verbatim}
        :- module(bar, [main/1]).
        :- include(library(persdb_sql)).
@end{verbatim}
@item{In a @em{user} file:}
@begin{verbatim}
	:- use_package([persdb_sql]).
@end{verbatim}
        or
@begin{verbatim}
        :- include(library(persdb_sql)).
@end{verbatim}
@end{description}
   This loads the run-time and compile-time versions of the library
   (@tt{persdbtr_sql.pl} and @tt{persdbrt_sql.pl}) and includes some
   needed declarations.").

:- comment(title,"SQL persistent database interface").

:- comment(subtitle,"@bf{The CIAO System Documentation Series}").
:- comment(subtitle,"Technical Report CLIP 10/98.0").
:- comment(subtitle,"RadioWeb (ESPRIT Project 25562) Report D3.1.M2-A2").

:- comment(subtitle,"ECCOSIC (@em{Comision Conjunta Hispano-Norteamericana}").
:- comment(subtitle,"@em{de Cooperacion Cientifica y Tecnologica} Project 98059)"). 
%% :- comment(subtitle,"@em{Draft printed on:} @today{}").

:- comment(subtitle,"December 26, 1998").

:- comment(author,
        "I. Caballero, D. Cabeza, J.M. G@'{o}mez, and M. Hermenegildo").
:- comment(author, "@tt{clip@@dia.fi.upm.es}").
:- comment(author, "@tt{http://www.clip.dia.fi.upm.es/}").
:- comment(author, "The CLIP Group").
:- comment(author, "Facultad de Inform@'{a}tica").
:- comment(author, "Universidad Polit@'{e}cnica de Madrid").

%% The Free Software Foundation.
:- comment(copyright,"
Copyright @copyright{} 1996-2002 The CLIP Group

@include{/home/clip/Systems/ciao/doc/common/Copyright.Manuals}
").

:- comment(summary,"This library implements the SQL-based version of
   the generic concept of @em{persistent predicates}. As introduced in
   the filebased version documentation (the @lib{persdb} library), a
   persistent predicate is a relation such that any updates made to it
   from a program remain after the execution of that program
   terminates.  In this case, persistence is achieved by saving the
   definition and state of the predicates in a @em{relational
   database}. This way, a very natural and high-level interface is
   provided for accessing SQL database relations from a Prolog
   program. Facilities are also provided for reflecting more complex
   @em{views} of the database relations as Prolog predicates. Such
   views can be constructed as conjunctions, disjunctions,
   projections, etc. of database relations, and may include SQL-like
   aggregation operations.").

:- comment(module,"The purpose of this library is to implement an
   instance of the generic concept of @concept{persistent predicates},
   where external @concept{relational databases} are used for storage
   (see the documentation of the @lib{persdb} library and
   @cite{radioweb-D3.1.M1-A1,radioweb-ta} for details).  To this end,
   this library exports SQL persistent versions of the
   @pred{assertz_fact/1}, @pred{retract_fact/1} and
   @pred{retractall_fact/1} builtin predicates.  Persistent predicates
   also allow @concept{concurrent updates} from several programs,
   since each update is atomic.

   The notion of persistence provides a very natural and transparent
   way to access database relations from a Prolog program. Stub
   definitions are provided for such predicates which access the
   database when the predicate is called (using the @lib{db_client}
   library).  A @concept{Prolog to SQL translator} is used to generate
   the required @concept{SQL} code dynamically (see library
   @lib{pl2sq_sql}).

   This library also provides facilities for reflecting more complex
   @concept{views} of the database relations as Prolog
   predicates. Such views can be constructed as conjunctions,
   disjunctions, projections, etc. of database relations. Also,
   @concept{SQL}-like @concept{aggregation operations} are supported.

@section{Implementation of the Database Interface}

The architecture of the low-level implementation of the database
interface was defined with two goals in mind:

@begin{itemize}
@item to simplify the communication between the Prolog system and the
  relational database engines as much as possible, and

@item to give as much flexibility as possible to the overall system.
  This includes simultaneous access to several databases, allowing
  both the databases and clients to reside on the same physical
  machine or different machines, and allowing the clients to reside in
  Win95/NT or Unix machines.
@end{itemize}


In order to allow the flexibility mentioned above, a client-sever
architecture was chosen. The following figure depicts the overall
architecture of the system:

@image{architecture}

At the server side, a ``database mediator server'' connects on one
side to the databases using the ODBC interface (this interface is
available for the databases of the RadioWeb project, as well as for
the majority of the databases running in the Win95/NT operating
systems) and on the other it is connected to the network by TCP/IP
using a fixed socket number / service (currently fixed to socket
number 2020).

The mediator server must run on the Windows (NT/95) operating system,
on the machine where the databases are also running. The (Prolog)
clients which connect to it can be run locally at the server machine.
In addition, remote clients running on different machines can also
connect to the mediator server by connecting to its socket number
(service).  Such clients can run on either Unix or Windows systems.

After the connection is established a client can send commands to the
mediator server which will pass them to the corresponding database
server, and then the data will traverse in the opposite direction.
These messages include logging on and off from the database, sending
SQL queries, and receiving the responses.

The low level implementation of the current library is accomplished by
providing several abstraction levels over the socket interface library
of the Prolog engine. These layers of abstraction implement the
persistent predicate view, build the appropriate commands for the
database using a translator of Prolog goals to SQL commands, issue
such commands using the mediator send/receive procedures, parse the
responses, and present such responses to the Prolog engine via
backtracking.

@section{Example(s)}

@begin{verbatim}
@includeverbatim{/home/clip/Systems/ciao/library/persdb_sql/examples/persdb_example.pl}
@end{verbatim}
   ").

%% ---------------------------------------------------------------------------

:- new_declaration(sql_persistent/3,yes).

:- comment(doinclude,sql_persistent/3).
:- decl sql_persistent(PrologPredTypes,TableAttributes,Keyword)
   => prologPredTypes * tableAttributes * persLocId

# "Declares the predicate corresponding to the main functor of
   @var{PrologPredTypes} as SQL persistent. @var{Keyword} is the
   @concept{name of a location} where the @concept{persistent storage}
   for the predicate is kept, which in this case must be an external
   relational database. The description of this database is given
   through the @pred{sql_persistent_location} predicate, which must
   contain a fact in which the first argument unifies with
   @var{Keyword}. @var{TableAttributes} provides the @concept{table
   name} and @concept{attributes} in the database corresponding
   respectively to the predicate name and arguments of the (virtual)
   Prolog predicate.

   @bf{Example:}

@begin{verbatim}
:- sql_persistent(product( integer,    integer, string, string ),
              product( quantity,   id,      name,   size   ),
              radiowebdb).

sql_persistent_location(radiowebdb,
   db('SQL Anywhere 5.0 Sample', user, pass,
      'r2d5.dia.fi.upm.es':2020)).
@end{verbatim}
".

%% ---------------------------------------------------------------------------

:- comment(prologPredTypes/1,"@includedef{prologPredTypes/1}").

:- prop prologPredTypes(PredTypes) # "@var{PredTypes} is a structure
    describing a Prolog predicate name with its types.".

prologPredTypes(PredTypes) :-
	PredTypes =.. [PredName|Types],
	atm(PredName),
 	list(Types,sqltype).

:- comment(tableAttributes/1,"@includedef{tableAttributes/1}").

:- prop tableAttributes(TableAttributes)  # "@var{TableAttributes} is a
    structure describing a table name and some attributes.".

tableAttributes(TableAttributes) :-
 	TableAttributes =.. [TableName|AttributeNames],
 	atm(TableName),
 	list(AttributeNames,atm).

:- prop persLocId(Id)  # "@var{Id} is the name of a persistent storage location.".

persLocId(Id) :-
 	atm(Id).

%% ---------------------------------------------------------------------------

:- comment(sql_persistent_location/2,"Relates names of locations
   (the @var{Keyword}s) with descriptions of such locations
   (@var{Location}s).").

:- pred sql_persistent_location(Keyword,DBLocation) =>  persLocId * database_desc

# "In this usage, @var{DBLocation} is a @em{relational database}, in which
   case the predicate is stored as tuples in the database.".

:- regtype database_desc(D) # "@var{D} is a structure describing a
   database.".

database_desc(db(DBId,User,Passwd,Socket)) :-
	dbname(DBId),
	user(User),
	passwd(Passwd),
	socketname(Socket).

:- comment(database_desc/1,"@includedef{database_desc/1}").

%% Imported from db_client.
:- comment(doinclude,dbname/1).
:- comment(doinclude,user/1).
:- comment(doinclude,passwd/1).
:- comment(doinclude,socketname/1).

:- comment(hide,relation/3).
:- comment(hide,attribute/4).
:- multifile([relation/3,attribute/4]).
:- data([relation/3,attribute/4]).

:- meta_predicate make_sql_persistent(addmodule,?,?).
:- impl_defined(make_sql_persistent/3).

:- pred make_sql_persistent(PrologPredTypes,TableAttributes,Keyword)
   => prologPredTypes * tableAttributes * persLocId

# "Dynamic version of the @decl{sql_persistent/3} declaration.".

make_sql_persistent(PrologDef, Mod, SQLDef, DBId) :-
	PrologDef =.. [PrologName | Types],
	SQLDef    =.. [TableName  | ArgNames],
	functor(PrologDef, PrologName, Arity),
	assertz_fact(relation(PrologName,Arity,TableName)),
	assert_args(Types,ArgNames,1,TableName),
	assertz_fact(sql_persistent(PrologName/Arity,DBId)),
	module_concat(Mod, PrologName, ModPrologName),
	functor(Consequence, PrologName, Arity),
	functor(Head, ModPrologName, Arity),
	equal_args(Head, Consequence, 1, Arity),
%	dynamic(PrologName/Arity),
	this_module(This),
	module_concat(This, db_call_db_atomic_goal(DBId, Consequence), Body),
	term_to_meta((Head :- Body), Clause),
	assertz(Clause).

equal_args(_Pred1, _Pred2, Index, Arity):- Index > Arity, !.
equal_args(Pred1, Pred2, Index, Arity) :-
	arg(Index, Pred1, Arg),
	arg(Index, Pred2, Arg),
	Index1 is Index + 1,
	equal_args(Pred1, Pred2, Index1, Arity).

assert_args([], [], _, _) :-
	!.
assert_args([Type|Ts], [ArgName|As], N, TableName) :-
	sqltype(Type),
	!,
	assertz_fact(attribute(N, TableName, ArgName, Type)),
	N1 is N+1,
	assert_args(Ts, As, N1, TableName).
assert_args([T|_], [_|_], _, TableName) :-
	!,
	error_message("illegal SQL type ~w in predicate for ~w",[T, TableName]).
assert_args(_,_,_,TableName) :-
	error_message("arity mismatch in declaration for ~w",[TableName]).


%% ---------------------------------------------------------------------------

:- meta_predicate dbassertz_fact(addmodule,?).
:- impl_defined(dbassertz_fact/1).

:- pred dbassertz_fact(+Fact) :: fact

  # "Persistent extension of @pred{assertz_fact/1}: the current instance of
     @var{Fact} is interpreted as a fact (i.e., a relation tuple) and is
     added to the end of the definition of the corresponding predicate. If
     any integrity constraint violation is done (database stored
     predicates), an error will be displayed.  The predicate concerned must
     be statically (@decl{sql_persistent/3}) or dinamically
     (@pred{make_sql_persistent/3}) declared. Any uninstantiated variables in
     the @var{Fact} will be replaced by new, private variables.  @bf{Note:}
     @em{assertion of facts with uninstantiated variables not implemented at
     this time.}".

dbassertz_fact(Fact) :-
	functor(Fact,F,A),
	debug_message("fact to assert is ~w",[Fact]),
	init_sql_persdb,
        debug_message("checking if ~w/~w is persistent",[F,A]),
 	sql_persistent(F/A,DBId),
 	debug_message("persistent predicate found, location: ~w",[DBId]),
	debug_message("ready to call to the insertion compiler, with the fact ~w",[Fact]),
	pl2sqlInsert(Fact,SQLString),
	debug_message("SQL insertion sentence is ~s",[SQLString]),
	sql_query(DBId,SQLString,ResultTerm),
	( ResultTerm='ok' ->
	      debug_message("Persistent fact inserted in the database",[])
	    ;
	      error_message("in insertion. Answer received is ~w",[ResultTerm])
	).
%% ---------------------------------------------------------------------------

:- meta_predicate dbretract_fact(addmodule,?).
:- impl_defined(dbretract_fact/1).

:- pred dbretract_fact(+Fact) :: fact

 # "Persistent extension of @pred{retract_fact/1}: deletes on
    backtracking all the facts which unify with @var{Fact}.  The
    predicate concerned must be statically (@decl{sql_persistent/3}) or
    dinamically (@pred{make_sql_persistent/3}) declared.".

dbretract_fact(Fact):-
        functor(Fact,F,A),
	debug_message("fact to retract is ~w",[Fact]),
	init_sql_persdb,
 	sql_persistent(F/A,DBId),
 	debug_message("persistent predicate found, location: ~w",[DBId]),
        debug_message("checking if ~w is a current fact",[Fact]),
        dbcurrent_fact(Fact), %% fails if Fact is not a current fact
	debug_message("~w found as a current fact",[Fact]),
	debug_message("calling odbc_delete, with the fact ~w",[Fact]),
%%  	           pl2sqlDelete(FactToRetract,SQLString),** we don't use
%%                    the SQL deletion compiler because SQL Anywhere doesn't
%%                    admit aliases in delete statements (SQL Anywhere 5.0
%%                    bug)
	odbc_delete(Fact,DBId),
	debug_message("ODBC deletion done. Persistent fact deleted from the database",[]).

%% ---------------------------------------------------------------------------

:- meta_predicate dbretractall_fact(addmodule,?).
:- impl_defined(dbretractall_fact/1).

:- pred dbretractall_fact(+Fact) :: fact

 # "Persistent extension of @pred{retractall_fact/1}: when called deletes
   all the facts which unify with @var{Fact}. The predicate concerned must
   be statically (@decl{sql_persistent/3}) or dinamically
   (@pred{make_sql_persistent/3}) declared.".

dbretractall_fact(Fact):-
	debug_message("fact to retract is ~w",[Fact]),
	init_sql_persdb,
	functor(Fact, F, A),
 	sql_persistent(F/A,DBId),
%% %%%%%% TO SEE: maybe there are two predicates with the same name
%%                and located in different places (different databases, files,
%%                persistent and non-persistent predicates,...)
 	debug_message("persistent predicate found, location: ~w",[DBId]),
	debug_message("calling odbc_delete, with the fact ~w",[Fact]),
	odbc_delete(Fact,DBId),
	debug_message("ODBC deletion done. Persistent fact deleted from the database",[]), fail.
dbretractall_fact(_).

%% TO SEE: keep odbc_delete as an internal predicate???
:- comment(hide,odbc_delete/2).

:- pred odbc_delete(+Fact,+DBId) :: atomicgoal * dbconnection

# "Internal predicate,used to delete the fact @var{Fact} from an external
  database. Not meant to be called directly by users.  @var{Fact} must be a
  call to a persistent predicate which resides in database @var{DbId}.  The
  current solution uses @lib{pl2sql_sql}. @pred{pl2sqlstring} provides a SQL
  select sentence, which is used to create a view and delete all its
  elements. Hence, user permission to create a view is needed.".

odbc_delete(Fact,DBId):-
%%%%%% TO MAKE: think about deleting complex predicates
	copy_term(Fact,FactDB),
	pl2sqlstring(FactDB,FactDB,SQLString),
	debug_message("Translated select sentence is ~s",[SQLString]),
%% Permission to create a view and to delete PL_TMP_TO_RETRACT is needed
	%% first we delete PL_TMP_TO_RETRACT, if it exists
        drop_temporal_view(DBId), %% drop the temporal view if it exists
	append("CREATE VIEW PL_TMP_TO_RETRACT AS ",SQLString,FirstStep),
	debug_message("First step to retract is the sentence ~s",[FirstStep]),
	sql_query(DBId,FirstStep,ResultTerm1),
	!,
	( ResultTerm1='ok' ->
	    debug_message("view created",[])
	  ;
	    error_message("creating view. Answer received is ~w",[ResultTerm1])
	),	
	delete_temporal_view_tuples(DBId),
	drop_temporal_view(DBId).

odbc_delete(_Fact,_DBId):- %% maybe not needed
	error_message(" Deletion couldn't be done ",[]),
	fail.

delete_temporal_view_tuples(DBId):-
	sql_query(DBId,"DELETE FROM PL_TMP_TO_RETRACT",ResultTerm),
	( ResultTerm='ok' ->
	    !,
	    debug_message("tuples deleted",[])
	  ;
	    error_message("deleting view tuples. Answer received is ~w",[ResultTerm])
	).

drop_temporal_view(DBId):-
	sql_query(DBId,"DROP VIEW PL_TMP_TO_RETRACT",ResultTerm1),
	( ResultTerm1='ok' ->
	    debug_message("view dropped",[])
	;     %% next lines are used to drop a view in Access ODBC Driver
	    %% trying to drop view with other SQL sentence. Temporarily useful,
            %% until deletion compiler adapted
	    sql_query(DBId,"DROP TABLE PL_TMP_TO_RETRACT",ResultTerm2),
	    ( ResultTerm2='ok' ->
	        debug_message("view (table) dropped",[])
	    ;
		debug_message("view PL_TMP_TO_RETRACT couldn't be dropped. Did it exists?",[])
	    )
	).

	

%% ---------------------------------------------------------------------------

:- meta_predicate dbcurrent_fact(addmodule,?).
:- impl_defined(dbcurrent_fact/1).

:- pred dbcurrent_fact(+Fact) :: fact

# "Persistent extension of @pred{current_fact/1}: the fact @var{Fact}
   exists in the current database.  The predicate concerned must be
   declared @decl{sql_persistent/3}.  Provides on backtracking all the
   facts (tuples) which unify with @var{Fact}.".

dbcurrent_fact(Fact) :-
	debug_message("fact to call is ~w",[Fact]),
	init_sql_persdb,
        functor(Fact, F, A),
%%%%%% TO MAKE: see if always 'user'. Better 'multifile'?
 	debug_message("checking if ~w/~w is persistent",[F,A]),	
	sql_persistent(F/A,DBId),
 	debug_message("persistent predicate found, location: ~w",[DBId]),
	debug_message("calling current_fact(~w) in database",[Fact]),
	db_call_db_atomic_goal(DBId,Fact).

%% ---------------------------------------------------------------------------

:- prop fact(X)  # "@var{X} is a fact (a term whose main functor is not @tt{':-'/2}).".

fact(_).

%% ---------------------------------------------------------------------------
%% Calling complex goals
%% ---------------------------------------------------------------------------

%% :- meta_predicate dbfindall(?,?,goal,?).

:- pred dbfindall(+DBId,+Pattern,+ComplexGoal,-Results)
   :: dbconnection * projterm * querybody * list

   # "Similar to @pred{findall/3}, but @var{Goal} is executed in
     database @var{DBId}. Certain restrictions and extensions apply to
     both @var{Pattern} and @var{ComplexGoal} stemming from the Prolog
     to SQL translation involved (see the corresponding type
     definitions for details).".

dbfindall(DBId,Pattern,ComplexGoal,Results) :-
	debug_message("projecting    ~w    onto    ~w    in ~w",
	       [ComplexGoal,Pattern,DBId]),
	init_sql_persdb,
	sql_persistent(_,DBId),
	!,
	db_query(DBId,Pattern,ComplexGoal,ResultsList),
	functor(Pattern,PF,_),
	map_pattern_functor(ResultsList,PF,Results).

dbfindall(DBId,_Pattern,ComplexGoal,_Results) :-
	error_message("in database identifier ~w in dbfindall/4 for ~w",
	       [DBId,ComplexGoal]),
	fail.

map_pattern_functor([],_PF,[]).
map_pattern_functor([IResult|IResults],PF,[OResult|OResults]) :-
	OResult =.. [PF|IResult],
	map_pattern_functor(IResults,PF,OResults).


%% ---------------------------------------------------------------------------

%% :- meta_predicate dbcall(?,goal).

:- pred dbcall(+DBId,+ComplexGoal) :: dbconnection * querybody

# "Internal predicate, used by the transformed versions of the
   persistent predicates. Not meant to be called directly by users. It
   is exported by the library so that it can be used by the
   transformed versions of the persistent predicates in the modules in
   which they reside. Sends @var{ComplexGoal} to database @var{DBId} for
   evaluation. @var{ComplexGoal} must be a call to a persistent predicate
   which resides in database @var{DBId}.".

dbcall(DBId,ComplexGoal) :-
	debug_message("calling ~w in ~w",[ComplexGoal,DBId]),
	init_sql_persdb,
	sql_persistent(_,DBId),
	!,
	varset(ComplexGoal,Vars),
	Pattern =.. [foo|Vars],
	debug_message("Calling db_query_one_tuple !~n",[]),
	db_query_one_tuple(DBId,Pattern,ComplexGoal,ResultsList),
	ResultsList = tup(Vars).

dbcall(DBId,ComplexGoal) :-
	error_message("in database identifier ~w in dbcall/2 for ~w",
	       [DBId,ComplexGoal]),
	fail.

%% ---------------------------------------------------------------------------

:- comment(doinclude,db_query/4).

:- pred db_query(+DBId,+ProjTerm,+Goal,ResultTerm)
	:: dbconnection * projterm * querybody * tuple

# "@var{ResultTerm} contains all the @concept{tuples} which are the
   response from database @var{DBId} to the Prolog query @var{Goal},
   projected onto @var{ProjTerm}. Uses @pred{pl2sqlstring/3} for the
   @concept{Prolog to SQL translation} and @pred{sql_query/3} for
   posing the actual query.".

db_query(DBId,ProjTerm,Goal,ResultTerm) :-
	copy_term(pair(ProjTerm,Goal),pair(DBProjTerm,DBGoal)),
	pl2sqlstring(DBProjTerm,DBGoal,SQLStringQuery),
	debug_message("sending SQL query ""~s"" ",[SQLStringQuery]),
%%	sql_query(DBId, SQLStringQuery, table(_,ResultsList) ),
	sql_query(DBId, SQLStringQuery, t(ResultTerm) ),
	debug_message("result is ~w",[ResultTerm]).

%% ---------------------------------------------------------------------------

:- comment(doinclude,sql_query/3).

:- pred sql_query(+DBId,+SQLString,AnswerTableTerm)
	:: dbconnection * sqlstring * answertableterm

# "@var{ResultTerm} is the response from database @var{DBId} to the
   @concept{SQL query} in @var{SQLString} to database
   @var{DBId}. @var{AnswerTableTerm} can express a set of tuples, an error
   answer or a 'ok' response (see @decl{answertableterm/1} for details).
   At the moment, @pred{sql_query/3} log in and out for each query. This
   should be changed to log in only the first time and log out on exit
   and/or via a timer in the standard way.".
%%%%%% TO MAKE: change this

sql_query(DBId,SQLString,ResultTerm):-
	sql_persistent_location(DBId,db(Id,User,Passwd,Address:Port)),
	odbc_connect(Address:Port,Stream),
	db_login(Stream,Id,User,Passwd,DbConnection),
	db_eval_sql(DbConnection,SQLString,ResultTerm),
	db_logoff(DbConnection),
	odbc_disconnect(Stream).

%% ---------------------------------------------------------------------------

:- comment(doinclude,db_query_one_tuple/4).

:- pred db_query_one_tuple(+DBId,+ProjTerm,+Goal,ResultTerm)
	:: dbconnection * projterm * querybody * answertupleterm

# "@var{ResultTerm} is one of the @concept{tuples} which are the response
   from database @var{DBId} to the Prolog query @var{Goal}, projected onto
   @var{ProjTerm}. Uses @pred{pl2sqlstring/3} for the @concept{Prolog to
   SQL translation} and @pred{sql_query_one_tuple/3} for posing the actual
   query. After last tuple has been reached, a null tuple is unified with
   ResultTerm, and the connection to the database finishes.".

db_query_one_tuple(DBId,ProjTerm,Goal,tup(ResultTerm)) :-
	copy_term(pair(ProjTerm,Goal),pair(DBProjTerm,DBGoal)),
	debug_message("calling pl2sqlstring",[]),
	pl2sqlstring(DBProjTerm,DBGoal,SQLStringQuery),!,
	debug_message("sending SQL query ""~s"" ",[SQLStringQuery]),
	sql_query_one_tuple(DBId, SQLStringQuery, ResultTerm),
	debug_message("result is ~w",[ResultTerm]).

%% ---------------------------------------------------------------------------
:- comment(doinclude,sql_query_one_tuple/3).

:- pred sql_query_one_tuple(+DBId,+SQLString,ResultTuple)
	:: dbconnection * sqlstring * tuple

# "@var{ResultTuple} contains an element from the set of tuples which
   represents the response in @var{DBId} to the @concept{SQL query}
   @var{SQLString}. If the connection is kept, succesive calls return
   consecutive tuples, until the last tuple is reached. Then a null tuple
   is unified with @var{ResultTuple} and the connection is finished (calls
   to @pred{db_logoff/1} and @pred{odbc_disconnect/1}).".

sql_query_one_tuple(DBId,SQLString,ResultTerm) :-
	sql_persistent_location(DBId,db(Id,User,Passwd,Address:Port)),
	odbc_connect(Address:Port,Stream),
	db_login(Stream,Id,User,Passwd,dbconnection(Stream,DbHandle)),
	db_stmt_handle(dbconnection(Stream,DbHandle),SQLString,dbqueryconnection(Stream,DbHandle,Answer)),
	debug_message("Processing db_stmt answer ~s",[Answer]),
	process_db_stmt_answer(Answer),
 %%%%%% TO MAKE: checking 'Answer' we have one call less
        sql_query_one_tuple_more(dbqueryconnection(Stream,DbHandle,Answer),tup(ResultTerm)).

sql_query_one_tuple_more(dbqueryconnection(Stream,DbHandle,Answer),tup(ResultTerm)) :-
	(
	  db_one_tuple(dbqueryconnection(Stream,DbHandle,Answer),tup(ResultTerm)),
	  (
	   ResultTerm = [] ->
	     db_logoff(dbconnection(Stream,DbHandle)),
	     odbc_disconnect(Stream),!
	  ;
	   true
	  )
	;
	  sql_query_one_tuple_more(dbqueryconnection(Stream,DbHandle,Answer),tup(ResultTerm))
	).

process_db_stmt_answer(Answer):-
	debug_message("Processing stmthandle ~s",[Answer]),
	match_string("ERROR",Answer,_Rest),
	debug_message("ERROR has been matched",[]),
	!,
	error_message("~s",[Answer]),
	fail.
process_db_stmt_answer(_Answer).

%% ---------------------------------------------------------------------------
%% Internal, for calling atomic goals only
%% ---------------------------------------------------------------------------

:- comment(hide,db_call_db_atomic_goal/2).

%% :- meta_predicate db_call_db_atomic_goal(?,goal).

:- pred db_call_db_atomic_goal(+DBId,+Goal) :: dbconnection * atomicgoal

# "Internal predicate, used by the transformed versions of the
   persistent predicates. Not meant to be called directly by users. It
   is exported by the library so that it can be used by the
   transformed versions of the persistent predicates in the modules in
   which they reside. Sends @var{Goal} to database @var{DBId} for
   evaluation. @var{Goal} must be a call to a persistent predicate
   which resides in database @var{DBId}.".

db_call_db_atomic_goal(DBId,Goal) :-
	debug_message("calling ~w in ~w",[Goal,DBId]),
	init_sql_persdb,
	functor(Goal,F,A),
	debug_message("checking if ~w/~w, stored in ~w, is persistent",[F,A,DBId]),
	(  sql_persistent(F/A, DBId)
	->
	   debug_message("Calling db_query_one_tuple for ~w/~w ~n",[F,A]),
	   db_query_one_tuple(DBId,Goal,Goal,ResultsList),
	   debug_message("Return from db_query_one_tuple with ~w ~n",[ResultsList]),
	   Goal =.. [_|OneResult],
	   ResultsList = tup(OneResult)
	   % member(OneResult,ResultsList)
	;  error_message("~w/~w is not a DB persistent predicate",[F,A]),
	   fail).

:- data db_started/0.

:- pred init_sql_persdb

# "Internal predicate, used to transform predicates statically declared as
   persistent (see @decl{sql_persistent/3}) into real persistent predicates. ".

init_sql_persdb:-
	db_started, !.
init_sql_persdb:-
	initialize_db,
	assertz_fact(db_started).

initialize_db:-
 	'$is_sql_persistent'(PrologDef,SQLDef,DBId),
 	make_sql_persistent(PrologDef,SQLDef,DBId),
	fail.
initialize_db.

:- comment(doinclude, dbconnection/1).
:- comment(doinclude, tuple/1).

:- prop atomicgoal(G)  # "@var{G} is an atomic goal.".

atomicgoal( G ) :-
	term(G).

:- comment(atomicgoal/1,"@var{G} is a single goal, not containing
   conjunctions, disjunctions, etc.").

%% ---------------------------------------------------------------------------

:- pred sql_get_tables(+Location,-Tables) :: persLocation * list(atm)

# "@var{Tables} contains the tables available in @var{Location}.".

:- pred sql_get_tables(+DbConnection,-Tables) :: dbconnection * list(atm)

# "@var{Tables} contains the tables available in @var{DbConnection}.".

sql_get_tables(db(Id,User,Passwd,Address:Port),TablesList):-
	!,
	odbc_connect(Address:Port,Stream),
	db_login(Stream,Id,User,Passwd,dbconnection(Stream,DbHandle)),
	db_get_tables(dbconnection(Stream,DbHandle),TablesList),
	db_logoff(dbconnection(Stream,DbHandle)),
	odbc_disconnect(Stream).

sql_get_tables(DBId, TablesList):-
	sql_persistent_location(DBId,db(Id,User,Passwd,Address:Port)),
	sql_get_tables(db(Id,User,Passwd,Address:Port), TablesList).

:- pred sql_table_types(+Location,+Table,-AttrTypes)
	:: persLocation * atm * list

# "@var{AttrTypes} are the attributes and types of @var{Table} in
   @var{Location}.".

:- pred sql_table_types(+DbConnection,+Table,-AttrTypes)
	:: dbconnection * atm * list

# "@var{AttrTypes} are the attributes and types of @var{Table} in
   @var{DbConnection}.".

sql_table_types(db(Id,User,Passwd,Address:Port),TableName,AttTypesList):-
        odbc_connect(Address:Port,Stream),
	db_login(Stream,Id,User,Passwd,dbconnection(Stream,DbHandle)),
	db_table_types(dbconnection(Stream,DbHandle),TableName, AttTypesNativeList),
	filter_types(AttTypesNativeList,AttTypesList),
	db_logoff(dbconnection(Stream,DbHandle)),
	odbc_disconnect(Stream).
	

sql_table_types(DBId, TableName, AttTypesList):-
	sql_persistent_location(DBId,db(Id,User,Passwd,Address:Port)),
	sql_table_types(db(Id,User,Passwd,Address:Port),TableName,AttTypesList).

%% Obtains the sqltype versions of the Native SQL System types retrieved
filter_types([],[]).
filter_types([[NativeId, NativeType]|NativeRest],[[NativeId,Type]|Rest]):-
	accepted_type(NativeType, Type),
	filter_types(NativeRest, Rest).
