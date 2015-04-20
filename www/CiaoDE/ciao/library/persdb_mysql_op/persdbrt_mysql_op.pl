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
%jcf-begin
	    sql_query_one_tuple/3,
	    replace_marks/4,
	    assert_args/4,
	    varlist/1,
%jcf-deterministic-db-calls
	    prefetch_db_call/2,
	    prefetch_sql_query_one_tuple/3,
%jcf-end
	    sql_table_types/3
        ],[assertions, regtypes, basicmodes, det_hook]).


%jcf-begin
%jcf%:- reexport(library('persdb_mysql/db_client_types'),
%jcf%	            [socketname/1,dbname/1,user/1,passwd/1]).
%jcf%:- reexport(library('persdb_mysql/pl2sql'),
%jcf%	            [projterm/1,querybody/1]).
%jcf%
%jcf%:- reexport(library('persdb_sql_common/sqltypes'),
%jcf%	            [sqltype/1]).
:- use_module(library('persdb_mysql_op/pl2sql'),
	[pl2sqlstring/3,sqlstring/1]).
:- reexport(library('persdb_mysql_op/pl2sql'),
	[projterm/1,querybody/1]).
:- reexport(library('persdb_mysql_op/sql_types'),
	[sql_type/1,accepted_type/2]).
:- reexport(library('persdb_mysql_op/db_client_types'),
	            [socketname/1,dbname/1,user/1,passwd/1]).
%jcf-end
%% sql_query_one_tuple_more/2 internal predicates

%% ---------------------------------------------------------------------------

%jcf% :- pred sql_persistent_location(Keyword,Location) ::  persLocId * persLocation
%jcf% 
%jcf% # "@var{Keyword} is an identifier for the persistent data location
%jcf%   @var{Location}.".
%jcf% 
%jcf% :- regtype persLocation/1.
%jcf% 
%jcf% persLocation(db(Name, User, Password, Machine:Port)) :-
%jcf% 	atm(Name),
%jcf% 	atm(User),
%jcf% 	atm(Password),
%jcf% 	atm(Machine),
%jcf% 	int(Port).

%% ---------------------------------------------------------------------------
%% Multifile predicates.

:- comment(sql_persistent_location/2,"Relates names of locations
   (the @var{Keyword}s) with descriptions of such locations
   (@var{Location}s).").

:- pred sql_persistent_location(Keyword,DBLocation) =>  persLocId * database_desc

# "In this usage, @var{DBLocation} is a @em{relational database}, in which
   case the predicate is stored as tuples in the database.".

:- multifile sql_persistent_location/2.
%:- data sql_persistent_location/2.
:- dynamic sql_persistent_location/2.

%% -----------------------------------------
:- comment(hide,'$is_sql_persistent'/3).

:- multifile '$is_sql_persistent'/3.
:- data sql_persistent/2.

%% -----------------------------------------
:- comment(hide,sql__relation/3).
:- comment(hide,sql__attribute/4).
:- multifile([sql__relation/3,sql__attribute/4]).
:- data([sql__relation/3,sql__attribute/4]).


%% ---------------------------------------------------------------------------
%% Properties and regular types.

:- regtype database_desc(D) # "@var{D} is a structure describing a
   database.".

database_desc(db(DBId,User,Passwd,Socket)) :-
	dbname(DBId),
	user(User),
	passwd(Passwd),
	socketname(Socket).

:- comment(database_desc/1,"@includedef{database_desc/1}").

:- regtype tuple(T) # "@var{T} is a tuple of values from the ODBC database
   interface.".

tuple(T) :-
	list(T,atm).

:- comment(tuple/1,"@includedef{tuple/1}").
:- comment(doinclude, tuple/1).

:- comment(doinclude, dbconnection/1).

:- regtype answertableterm(AT) # "@var{AT} is a response from the ODBC
   database interface.".

answertableterm(ok).
answertableterm(t(Answers)) :-
	list(Answers,tuple).
answertableterm(err(Answer)) :-
	term(Answer).	

:- comment(answertableterm/1,"@includedef{answertableterm/1}").

:- regtype answertupleterm(X) #  "@var{X} is a predicate containing a tuple.".

answertupleterm([]).
answertupleterm(tup(T)) :-
	tuple(T).

:- comment(answertupleterm/1,"@includedef{answertupleterm/1}").

%% Imported from db_client.
:- comment(doinclude,dbname/1).
:- comment(doinclude,user/1).
:- comment(doinclude,passwd/1).
:- comment(doinclude,socketname/1).

:- comment(prologPredTypes/1,"@includedef{prologPredTypes/1}").

:- prop prologPredTypes(PredTypes) # "@var{PredTypes} is a structure
    describing a Prolog predicate name with its types.".

prologPredTypes(PredTypes) :-
	PredTypes =.. [PredName|Types],
	atm(PredName),
%jcf-begin
%jcf% 	list(Types,sqltype).
	list(Types,sql_type).
%jcf-end

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

:- prop fact(X)  # "@var{X} is a fact (a term whose main functor is not @tt{':-'/2}).".

fact(_).

:- prop atomicgoal(G)  # "@var{G} is an atomic goal.".

atomicgoal( G ) :-
	term(G).

:- comment(atomicgoal/1,"@var{G} is a single goal, not containing
   conjunctions, disjunctions, etc.").


%-----------------------------------------------------------------------------
:- use_module(library(dynamic)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(terms_vars), [varset/2]).
:- use_module(library(messages),[error_message/2,debug_message/2]).
:- use_module(library(lists),[length/2,append/3]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(engine(internals),[term_to_meta/2,module_concat/3]).

:- use_module(library('persdb_mysql_op/mysql_client')).

%jcf-begin
%jcf%:- use_module(library('persdb_mysql/pl2sql'),
%jcf%	[pl2sqlstring/3,sqlstring/1
%jcf%	%% ,sqltype/1
%jcf%	]).
%jcf%:- use_module(library('persdb_sql_common/sqltypes'),
%jcf%	[accepted_type/2
%jcf%	%% ,sqltype/1
%jcf%	]).
%jcf-end
:- use_module(library('persdb_sql_common/pl2sqlinsert'),
	[pl2sqlInsert/2]).
%jcf-begin
%jcf%:- use_module(library('persdb_mysql/delete_compiler/pl2sqldelete'),
%jcf%	[pl2sqlDelete/2]).
:- use_module(library('persdb_mysql_op/pl2sqldelete'),[pl2sqlDelete/2]).
%jcf-end

:- comment(bug,"At least in the shell, reloading a file after changing
   the definition of a persistent predicate does not eliminate the old
   definition...").

:- comment(bug,"Functionality missing: some questions need to be debugged.").

:- comment(bug,"Warning: still using kludgey string2term and still
   using some non-uniquified temp files.").

:- comment(bug,"Needs to be unified with the file-based library.").

%jcf%:- use_module(library(format)).
%jcf%:- set_prolog_flag(write_strings,on).
%jcf%jcf_message(S):-
%jcf%	open('/home/jcorreas/cvs/Systems/Amos/DataBase/test.sql',append,Stream),
%jcf% 	format(Stream,"~s~n",[S]),
%jcf% 	flush_output(Stream),
%jcf% 	close(Stream).


%% ---------------------------------------------------------------------------
%% Intro
%% ---------------------------------------------------------------------------

:- comment(doinclude, projterm/1).

:- comment(doinclude, querybody/1).


:- comment(usage, "Typically, this library is used including the
   'persdb_mysql' package into the package list of the module, or using the
   @decl{use_package/1} declaration:
@begin{description}
@item{In a module:}
@begin{verbatim}
	:- module(bar, [main/1], [persdb_mysql]).
@end{verbatim}
        or
@begin{verbatim}
        :- module(bar, [main/1]).
        :- include(library(persdb_mysql)).
@end{verbatim}
@item{In a @em{user} file:}
@begin{verbatim}
	:- use_package([persdb_mysql]).
@end{verbatim}
        or
@begin{verbatim}
        :- include(library(persdb_mysql)).
@end{verbatim}
@end{description}
   This loads the run-time and compile-time versions of the library
   (@tt{persdbtr_mysql.pl} and @tt{persdbrt_mysql.pl}) and includes some
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
        "I. Caballero, D. Cabeza, J.M. G@'{o}mez, M. Hermenegildo, J. F. Morales, and M. Carro").
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
   the generic of concept @em{persistent predicates}. As introduced in
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
   @lib{pl2sql}).

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
architecture was chosen.  At the server side, a MySQL server connects
to the databases using the MySQL.  At the client side, a MySQL client
interface connects to this server.  The server daemon (mysqld) should
be running in the server machine; check your MySQL documentation on
how to do that.

After the connection is established a client can send commands to the
mediator server which will pass them to the corresponding database
server, and then the data will traverse in the opposite direction.
These messages include logging on and off from the database, sending
SQL queries, and receiving the responses.

The low level implementation of the current library is accomplished by
providing abstraction levels over the MySQL interface library. These
layers of abstraction implement the persistent predicate view, build
the appropriate commands for the database using a translator of Prolog
goals to SQL commands, issue such commands using the mediator
send/receive procedures, parse the responses, and present such
responses to the Prolog engine via backtracking.

@section{Example(s)}

@begin{verbatim}
@includeverbatim{/home/clip/Systems/ciao/library/persdb_mysql/examples/people.pl}
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

   Although a predicate may be persistent, other usual clauses can be
   defined in the source code. When querying a persistent predicate
   with non-persistent clauses, persistent and non-persisten clauses
   will be evaluated in turn; the order of evaluation is the usual
   Prolog order, considering that persistent clauses are defined in
   the program point where the @decl{sql_persistent/3} declaration is.

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


:- meta_predicate make_sql_persistent(addmodule,?,?).
:- impl_defined(make_sql_persistent/3).

:- pred make_sql_persistent(PrologPredTypes,TableAttributes,Keyword)
   => prologPredTypes * tableAttributes * persLocId

# "Dynamic version of the @decl{sql_persistent/3} declaration.".

make_sql_persistent(PrologDef, Mod, SQLDef, DBId) :-
	PrologDef =.. [PrologName | Types],
	SQLDef    =.. [TableName  | ArgNames],
	functor(PrologDef, PrologName, Arity),
	assertz_fact(sql__relation(PrologName,Arity,TableName)),
	assert_args(Types,ArgNames,1,TableName),
	assertz_fact(sql_persistent(PrologName/Arity,DBId)),
	% Next is only useful when making a sql_persistent
	% predicate dynamically (statically it has no effect).
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
%jcf-begin
%jcf%	sqltype(Type),
	sql_type(Type),
%jcf-end
	!,
	assertz_fact(sql__attribute(N, TableName, ArgName, Type)),
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
%jcf% 	jcf_message(SQLString),
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
	debug_message("calling mysql_delete, with the fact ~w",[Fact]),
%%  	           pl2sqlDelete(FactToRetract,SQLString),** we don't use
%%                    the SQL deletion compiler because SQL Anywhere doesn't
%%                    admit aliases in delete statements (SQL Anywhere 5.0
%%                    bug)
	mysql_delete(Fact,DBId),
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
%        asserta_fact(issue_debug_messages(persdb_mysql)),
	message("fact to retract is ~w",[Fact]),
	init_sql_persdb,
	functor(Fact, F, A),
 	sql_persistent(F/A,DBId),
%% %%%%%% TO SEE: maybe there are two predicates with the same name
%%                and located in different places (different databases, files,
%%                persistent and non-persistent predicates,...)
 	debug_message("persistent predicate found, location: ~w",[DBId]),
	debug_message("calling mysql_delete, with the fact ~w",[Fact]),
	mysql_delete(Fact,DBId),
	debug_message("ODBC deletion done. Persistent fact deleted from the database",[]), fail.
dbretractall_fact(_).

%% TO SEE: keep mysql_delete as an internal predicate???
:- comment(hide,mysql_delete/2).

:- pred mysql_delete(+Fact,+DBId) :: atomicgoal * dbconnection

# "Internal predicate,used to delete the fact @var{Fact} from an external
  database. Not meant to be called directly by users.  @var{Fact} must be a
  call to a persistent predicate which resides in database @var{DbId}.  The
  current solution uses @lib{pl2sql}. @pred{pl2sqlstring} provides a SQL
  select sentence, which is used to create a view and delete all its
  elements. Hence, user permission to create a view is needed.".

mysql_delete(Fact,DBId):-
%%%%%% TO MAKE: think about deleting complex predicates
	copy_term(Fact,FactDB),
	pl2sqlDelete(FactDB,SQLString),
	debug_message("Translated select sentence is ~s",[SQLString]),
%jcf% 	jcf_message(SQLString),
%% Permission to create a view and to delete PL_TMP_TO_RETRACT is needed
	%% first we delete PL_TMP_TO_RETRACT, if it exists
	sql_query(DBId,SQLString,ResultTerm),
	!,
	( ResultTerm='ok' ->
	    debug_message("deleted",[])
	  ;
	    error_message("delete failed. Answer received is ~w",[ResultTerm])
	).

mysql_delete(_Fact,_DBId):- %% maybe not needed
	error_message(" Deletion couldn't be done ",[]),
	fail.

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
%jcf-deterministic-db-calls-begin
%jcf%	db_call_db_atomic_goal(DBId,Fact).
	prefetch_db_call(DBId,Fact).
%jcf-deterministic-db-calls-end

%% ---------------------------------------------------------------------------
%% Calling complex goals
%% ---------------------------------------------------------------------------

:- meta_predicate dbfindall(?,?,goal,?).

:- pred dbfindall(+DBId,+Pattern,+ComplexGoal,-Results)
   :: dbconnection * projterm * querybody * list

   # "Similar to @pred{findall/3}, but @var{Goal} is executed in
     database @var{DBId}. Certain restrictions and extensions apply to
     both @var{Pattern} and @var{ComplexGoal} stemming from the Prolog
     to SQL translation involved (see the corresponding type
     definitions for details).".

dbfindall(_, Pattern, ComplexGoal, Results) :-
	findall(Pattern, ComplexGoal, Results).

% SINCE MySQL DOES NOT SUPPORT NESTED SELECTS THIS CODE WILL NOT
% WORK. USE findall/3 INSTEAD.
%
%dbfindall(DBId,Pattern,ComplexGoal,Results) :-
%	debug_message("projecting    ~w    onto    ~w    in ~w",
%	       [ComplexGoal,Pattern,DBId]),
%	init_sql_persdb,
%	sql_persistent(_,DBId),
%	!,
%	db_query(DBId,Pattern,ComplexGoal,ResultsList),
%	functor(Pattern,PF,_),
%	map_pattern_functor(ResultsList,PF,Results).
%
%dbfindall(DBId,_Pattern,ComplexGoal,_Results) :-
%	error_message("in database identifier ~w in dbfindall/4 for ~w",
%	       [DBId,ComplexGoal]),
%	fail.
%
%map_pattern_functor([],_PF,[]).
%map_pattern_functor([IResult|IResults],PF,[OResult|OResults]) :-
%	OResult =.. [PF|IResult],
%	map_pattern_functor(IResults,PF,OResults).


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
%jcf% 	jcf_message(SQLStringQuery),
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
	sql_persistent_location(DBId,db(Id,User,Passwd,Socket)),
	mysql_connect(Socket,Id,User,Passwd,DbConnection),
	( mysql_query(DbConnection,SQLString,ResultTerm) ->
           true
        ; atom_codes(SQLStringAtom,SQLString),
	  atom_concat('in answer to ',SQLStringAtom,Error),
	  ResultTerm = err(Error)
	),
	mysql_disconnect(DbConnection).

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
%jcf% 	jcf_message(SQLStringQuery),
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
   to @pred{mysql_disconnect/1}).".

sql_query_one_tuple(DBId,SQLString,ResultTerm) :-
	sql_persistent_location(DBId,db(Id,User,Passwd,Socket)),
	mysql_connect(Socket,Id,User,Passwd,DbConnection),
	mysql_query_one_tuple(DbConnection,SQLString,DbQueryConnection),
	Try = fetch_nd(DbConnection,DbQueryConnection,ResultTerm),
	OnCut = fetch_nd_cut(DbConnection,DbQueryConnection),
	OnFail = fetch_nd_fail(DbConnection,DbQueryConnection),
	det_try(Try, OnCut, OnFail).

fetch_nd(DbConnection,DbQueryConnection,ResultTerm) :-
	mysql_fetch(DbQueryConnection,tup(ResultTerm0)), \+ ResultTerm0 = [],
	( ResultTerm0 = ResultTerm ;
	  fetch_nd(DbConnection,DbQueryConnection,ResultTerm) ).

fetch_nd_cut(DbConnection,DbQueryConnection) :-
	mysql_free_query_connection(DbQueryConnection),
        mysql_disconnect(DbConnection).

fetch_nd_fail(DbConnection,DbQueryConnection) :-
	mysql_free_query_connection(DbQueryConnection),
        mysql_disconnect(DbConnection).

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

%---------------------------------------------------------
%jcf-deterministic-db-calls-begin
:- use_module(library(odd),[undo/1]).
:- meta_predicate undo_goal(goal).
undo_goal('$:'(Goal)) :-
	undo(Goal).

:- use_module(library(prolog_sys),[new_atom/1]).

:- pred prefetch_db_call(+CallKey,+DBId,+Goal) :: atm * dbconnection * atomicgoal

# "Modified version of @tt{db_call_db_atomic_goal/2} to check
  deterministic goals at run-time. This allows to close database
  connections when there are no remaining tuples.  Not meant to be
  called directly by users. It is exported by the library so that it
  can be used by the transformed versions of the persistent predicates
  in the modules in which they reside.".

prefetch_db_call(DBId,Goal):-
	debug_message("prefetch_db_call: calling ~w in ~w",[Goal,DBId]),
	init_sql_persdb,
	functor(Goal,F,A),
	debug_message("prefetch_db_call: checking if ~w/~w, stored in ~w, is persistent",[F,A,DBId]),
	(  sql_persistent(F/A, DBId)
	->
	   debug_message("prefetch_db_call: Calling db_query_one_tuple for ~w/~w ~n",[F,A]),
	   prefetch_db_query_one_tuple(DBId,Goal,Goal,ResultsList),
	   debug_message("prefetch_db_call: Return from db_query_one_tuple with ~w ~n",[ResultsList]),
	   Goal =.. [_|OneResult],
	   ResultsList = tup(OneResult)
	   % member(OneResult,ResultsList)
	;  error_message("~w/~w is not a DB persistent predicate",[F,A]),
	   fail).

prefetch_db_query_one_tuple(DBId,ProjTerm,Goal,tup(ResultTerm)) :-
	copy_term(pair(ProjTerm,Goal),pair(DBProjTerm,DBGoal)),
	debug_message("prefetch_db_query_one_tuple: calling pl2sqlstring",[]),
	pl2sqlstring(DBProjTerm,DBGoal,SQLStringQuery),!,
	debug_message("prefetch_db_query_one_tuple: sending SQL query ""~s"" ",[SQLStringQuery]),
%jcf% 	jcf_message(SQLStringQuery),
	prefetch_sql_query_one_tuple(DBId, SQLStringQuery, ResultTerm),
	debug_message("prefetch_db_query_one_tuple: result is ~w",[ResultTerm]).


:- pred prefetch_sql_query_one_tuple(+DBId,+SQLString,ResultTuple) 
	:: atm * dbconnection * sqlstring * tuple

# "Modified version of @tt{sql_query_one_tuple/3} to check
  deterministic goals at run-time.".

prefetch_sql_query_one_tuple(DBId,SQLString,ResultTerm):-
	% It always uses mysql_use_results
	new_call_key(CallKey),
	sql_persistent_location(DBId,db(Id,User,Passwd,Socket)),
	debug_message("prefetch_sql_query_one_tuple:  ~w",[sql_persistent_location(DBId,db(Id,User,Passwd,Socket))]),
	mysql_connect(Socket,Id,User,Passwd,DbConnection),
	assertz_fact(prefetch_connected(CallKey)),
%% only for debugging
%%	findall(X,prefetch_connected(X),L), length(L,N),
%%	findall((Y,Z),prefetched_answer(Y,Z),L2), length(L2,N2),
%%	debug_message("prefetch_sql_query_one_tuple: Number of connected queries ~w",[N]),
%%	debug_message("prefetch_sql_query_one_tuple: Total number of prefetched tuples  ~w",[N2]),
%%
	mysql_query_one_tuple(DbConnection,SQLString,DbQueryConnection),
	debug_message("prefetch_sql_query_one_tuple: after mysql_query_one_tuple",[]),
	undo(persdbrt_mysql_op:disconnect_if_not_disconnected(CallKey,DbQueryConnection,DbConnection)),
	retractall_fact(prefetched_answer(CallKey,_)),
	prefetch_window_size(K),
	repeat, % repeat while there are remaining answers.
	debug_message("prefetch_sql_query_one_tuple: after repeat",[]),
	prefetch(K,CallKey,DbConnection,DbQueryConnection),
	retract_fact(prefetched_answer(CallKey,ResultTerm)),
	debug_message("prefetch_sql_query_one_tuple: after retracting prefetched_answer(~w,~w)",[CallKey,ResultTerm]),
	( ResultTerm = '$fails' ->
	  !, fail
	; true
	).

prefetch_window_size(10). %actual size plus one.

prefetch(0,_CallKey,_DbConnection,_DbQueryConnection).
prefetch(K,CallKey,DbConnection,DbQueryConnection):-
	( fetch_nd(DbConnection,DbQueryConnection,ResultTerm) ->
	  assertz_fact(prefetched_answer(CallKey,ResultTerm)),
	  K1 is K - 1,
	  prefetch(K1,CallKey,DbConnection,DbQueryConnection)
	; assertz_fact(prefetched_answer(CallKey,'$fails')),
	  mysql_free_query_connection(DbQueryConnection),
	  mysql_disconnect(DbConnection),
	  retract_fact(prefetch_connected(CallKey))
	),
	!.

disconnect_if_not_disconnected(CallKey,DbQueryConnection,DbConnection):-
	debug_message("prefetch_sql_query_one_tuple: before disconnecting(~w,~w)",[CallKey,DbConnection]),
	retractall_fact(prefetched_answer(CallKey,_)),
	free_call_key(CallKey),
	debug_message("prefetch_sql_query_one_tuple: after freeing key ~w",[CallKey]),
	retract_fact(prefetch_connected(CallKey)),
	debug_message("prefetch_sql_query_one_tuple: after removing prefetch info ~w",[CallKey]),
	mysql_free_query_connection(DbQueryConnection),
	mysql_disconnect(DbConnection),
	debug_message("prefetch_sql_query_one_tuple: after disconnecting(~w)",[DbConnection]).

:- data recycled_atom/1.

% new_call_key(CallKey) generates a new query key to store prefetched
% tuples read from the database in advance.
%
% new_call_key/1 and free_call_key/1 predicates are needed (instead of
% using directly new_atom/1) because in massive database tasks the
% atom table may become full. 
% These predicates must be used carefully to avoid clashes between
% different database queries.
new_call_key(CallKey):-
	retract(recycled_atom(CallKey)).
new_call_key(CallKey):-
	new_atom(CallKey).

% free_call_key(CallKey) frees a query key to be used in a different
% query. This behaviour avoids the danger of fulling the atom table in 
% database-intensive programs.
free_call_key(CallKey):-
	assertz(recycled_atom(CallKey)).

:- pred prefetched_answer(+CallKey,+Goal) :: atm * atomicgoal

# "Data fact to store those database tuples read in advance by
  @tt{prefetch_db_call/3} or @tt{prefetch_sql_query_one_tuple/4}. If
  there is no next tuple, then the prefetched answer is the atom
  @tt{'$fails'}".

:- data prefetched_answer/2.
:- data prefetch_connected/1.
%jcf-deterministic-db-calls-end
%---------------------------------------------------------

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

%% ---------------------------------------------------------------------------

:- pred sql_get_tables(+Location,-Tables) :: database_desc * list(atm)

# "@var{Tables} contains the tables available in @var{Location}.".

:- pred sql_get_tables(+DbConnection,-Tables) :: dbconnection * list(atm)

# "@var{Tables} contains the tables available in @var{DbConnection}.".

sql_get_tables(db(Id,User,Passwd,Socket),TablesList) :- !,
	mysql_connect(Socket,Id,User,Passwd,DbConnection),
	mysql_get_tables(DbConnection,TablesList),
	mysql_disconnect(DbConnection).

sql_get_tables(DBId, TablesList):-
	sql_persistent_location(DBId, Location),
	sql_get_tables(Location, TablesList).

:- pred sql_table_types(+Location,+Table,-AttrTypes)
	:: database_desc * atm * list

# "@var{AttrTypes} are the attributes and types of @var{Table} in
   @var{Location}.".

:- pred sql_table_types(+DbConnection,+Table,-AttrTypes)
	:: dbconnection * atm * list

# "@var{AttrTypes} are the attributes and types of @var{Table} in
   @var{DbConnection}.".

sql_table_types(db(Id,User,Passwd,Socket),TableName,AttTypesList):-
	mysql_connect(Socket,Id,User,Passwd,DbConnection),
	mysql_table_types(DbConnection,TableName, AttTypesNativeList),
	filter_types(AttTypesNativeList,AttTypesList),
	mysql_disconnect(DbConnection).	

sql_table_types(DBId, TableName, AttTypesList):-
	sql_persistent_location(DBId,Location),
	sql_table_types(Location,TableName,AttTypesList).

%% Obtains the sqltype versions of the Native SQL System types retrieved
filter_types([],[]).
filter_types([[NativeId, NativeType]|NativeRest],[[NativeId,Type]|Rest]):-
	accepted_type(NativeType, Type),
	filter_types(NativeRest, Rest).
	
%% ---------------------------------------------------------------------------
%%jcf-begin
:- pred replace_marks(+GroundedGoal,+Goal,+SQLString0,-SQLString1) 

# "Given a SQL string with dummy values for ground variables
  @var{SQLString0}, provides the SQL string result of replacing dummy
  values by actual values @var{SQLString1}. Dummy values are in
  @var{GroundedGoal} term, while actual values are in the same
  position of @var{Goal}.".

replace_marks(GroundedGoal,Goal,SQLString0,SQLString1):-
	get_values(GroundedGoal,Goal,L),
	replace_marks_(SQLString0,L,SQLString1), !.

get_values(V1,V2,[(V1,V2)]):-
	atomic(V1),
	atom_concat('persdb_opt$',_,V1),!.
get_values(T1,T2,L):-
	functor(T1,Name,A),
	functor(T2,Name,A),
	A>0,!,
	get_values_(T1,T2,L,A).
get_values(_,_,[]).

get_values_(_,_,[],0).
get_values_(T1,T2,L,A):-
	arg(A,T1,Arg1),
	arg(A,T2,Arg2),
	get_values(Arg1,Arg2,L0),
	A1 is A - 1,
	get_values_(T1,T2,L1,A1),
	append(L0,L1,L).

replace_marks_([],_L,[]).
replace_marks_("'persdb_opt$"||Xs0,L,Ys):-
	get_to_quote(Xs0,As,Xs1),
	atom_codes(A,As),
	atom_concat('persdb_opt$',A,Mark),
	member((Mark,Value),L),
	to_string(Value,Values),
	replace_marks_(Xs1,L,Ys1),
	append(Values,Ys1,Ys).
replace_marks_([X|Xs],L,[X|Ys]):-
	replace_marks_(Xs,L,Ys).

get_to_quote([0''|Xs],[],Xs).
get_to_quote([X|Xs],[X|Ys],Zs):-
	get_to_quote(Xs,Ys,Zs).
	
to_string(X,Y):-
	atom(X),
	atom_concat(['\'',X,'\''],X0),
	atom_codes(X0,Y).
to_string(X,Y):-
	number(X),
	number_codes(X,Y).

:- pred varlist(+VarList) # ".".

varlist([]).
varlist([X|Xs]):-
	var(X),
	varlist(Xs).
%%jcf-end
