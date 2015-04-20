:- module(odbc_client,
	[
	    odbc_connect/5,
	       dbconnection/1,
	    odbc_query/3,
	    odbc_query_one_tuple/3,
	       dbqueryconnection/1,
            odbc_free_query_connection/1,
	    odbc_fetch/2,
	    odbc_get_tables/2,
	    odbc_table_types/3,
	    odbc_disconnect/1
	],[assertions,regtypes,basicmodes]).

:- use_module(db_client_types).

:- use_module(library(sockets),
     [connect_to_socket/3, socket_recv/2, socket_recv_code/3, socket_send/2]).

:- use_module(library(write),[write/1]).
:- use_module(library(strings),[get_line/1, write_string/1]).
:- use_module(library(lists),[append/3, list_concat/2]).
%% ,list/1,list/2]).

:- use_module(library(messages),[error_message/2,debug_message/2]).

% ----------------------------------------------------------------------------

:- comment(title,"Low-level socket interface to SQL/ODBC databases").

:- comment(author,"D. Cabeza, M. Carro, I. Caballero, and M. Hermenegildo.").

:- comment(module,"This library provides a socket-based interface to
   @concept{SQL Databases}, using the @concept{database mediator server}
   ODBC interface developed by C. Taboch and I. Caballero. The interface
   currently works for databases running in @concept{Win95/NT machine}s via
   @concept{ODBC}. This low-level interface was defined with two goals in
   mind:

   @begin{itemize}

   @item To simplify the communication between the Prolog system and
   the relational database engines as much as possible.

   @item To give as much flexibility to the overall system. This
   includes supporting simultaneous access to several databases, and
   also allowing both the databases and clients to reside on the same
   or different physical Win95/NT machines. The clients can reside
   also in @concept{Unix/Linux machine}s. It is also possible to
   access database selections in either @concept{tuple at a time} or
   @concept{set at a time} fashion, as needed.

   @end{itemize}

   In order to allow the flexibility mentioned above, a socket
   (TCP/IP) client-server architecture was chosen. The interface has
   two main components:

   @begin{itemize}

   @item At the server side, a @em{database mediator server} connects on
   one side to the databases using the ODBC interface and on the other to a
   fixed port number (service), on which the mediator server listens for
   requests.  The source code (and executables) for this server (slightly
   modified versions of C. Taboch original code) are in the same directory
   as this library. This executable must be running on the Windows (NT/95)
   machine where the databas(es) is(are) also running.@footnote{The server
   can be downloaded from the same site(s) as the Ciao system. The
   downloaded package contains complete installation instructions for the
   server. Before trying to run the Ciao Prolog side of things, make sure
   that the server is correctly installed and running, and that it can be
   accessed from the small sample client that comes with the server
   distribution. Once this is achieved, connection from Ciao should not
   pose problems.}

   @item At the client side a Prolog client can connect to the
   database by loading this library and calling the appropriate
   predicates. The Prolog client can run on either Windows or
   Unix/Linux systems, locally at the server machine or remotely in
   different machines. The mediator server port number (service) used
   is currently fixed to 2020.  After the connection is established a
   client can send commands to the mediator server which will pass
   them to the corresponding database server, and then the response
   data will return in the opposite direction. These messages include
   login on and off from the database, sending @concept{SQL queries},
   and receiving the responses.

   @end{itemize}

   @bf{Example:} 
@begin{verbatim}
@includeverbatim{/home/clip/Systems/ciao/library/persdb_sql/examples/db_client_example.pl}
@end{verbatim}
").

% ----------------------------------------------------------------------------

:- pred odbc_connect(+DbAddress,+DbName,+User,+Passwd,-DbConnection) 
   :: socketname * dbname * user * passwd * dbconnection

# "Logs on to the database @var{DbName} with user @var{User} and
   password @var{Passwd} via connection
   @var{Stream}. @var{DbConnection} contains the identifier of the
   session, to be used in the calls to other predicates defining the
   interface.
   It fails and display an error message if the login is not succesfully completed.".
%%%%%% TO SEE: 
%%  Note: uncommenting the following fact allows connecting to a database 
%% specifying only the data source name (for debugging).
login_without_userid_permitted.	

odbc_connect(DbAddress,DbName,User,Passwd,DbConnection):-
	DbAddress = Host:Port,
	connect_to_socket(Host, Port, Stream),
	odbc_connect_2(Stream,DbName,User,Passwd,DbConnection).
	
odbc_connect_2(Stream,DbName,User,Passwd,dbconnection(Stream,DbHandle)):-
	User\=='',
	atom_concat('LogonUser ',User,T1),
	atom_concat(T1,' ',T2),
	atom_concat(T2,Passwd,T3),
	atom_concat(T3,' ',T4),
	atom_concat(T4,DbName,SentenceAtom),
	atom_codes(SentenceAtom,SentenceString),
	null_ended_string(SentenceString,StringToLoginDb),
	debug_message("Logging on by sending:~s",[StringToLoginDb]),
	socket_send(Stream,StringToLoginDb),
	debug_message("Waiting for response...",[]),
	socket_recv_long(Stream,DbHand),
	null_ended_to_blank_ended(DbHand,DbHandle),
	debug_message("DbHandle received is: ~s",[DbHandle]),
	!,
	(   match_string("DbHandle",DbHandle,_Rest)
	->  asserta_fact(session(DbHandle,DbName,User))
	;   error_message("Couldn't logon.  ~s",[DbHandle]),
	    fail
	).
	
odbc_connect_2(Stream,DbName,User,Passwd,dbconnection(Stream,DbHandle)):- 
	User=='',Passwd=='',
	login_without_userid_permitted,
	atom_concat('Logon ',DbName,AtomToLoginDb),
	atom_codes(AtomToLoginDb,StringToLoginDb),
	null_ended_string(StringToLoginDb,StringToSend),
	debug_message("Logging on by sending: ~s",[StringToSend]),
	socket_send(Stream,StringToSend),
	debug_message("Waiting for response...",[]),
	socket_recv_long(Stream,DbHand),
	null_ended_to_blank_ended(DbHand,DbHandle),
	debug_message("DbHandle received is: ~s",[DbHandle]),
	!,
	(   match_string("DbHandle",DbHandle,_Rest) 
	->  asserta_fact(session(DbHandle,DbName,User))
	;   error_message("Couldn't logon.  ~s",[DbHandle]),
	    fail
	).

odbc_connect_2(Stream,_DbName,User,Passwd,dbconnection(Stream,_DbHandle)):- 
	User=='',Passwd=='',
	\+login_without_userid_permitted,
	!,
	error_message("login without user identifier not permitted",[]).

odbc_connect_2(_Stream,DbName,User,_Passwd,_) :- 
	error_message("login not completed for ~w@~w:",[User,DbName]),
	fail.

:- data session/3.

:- pred session(DbHandle,DbName,User) :: dbhandle * dbname * user 

# "Stores session info by relating @var{DbHandle} with @var{DbName}
  and @var{User}.".

% ----------------------------------------------------------------------------

:- regtype dbconnection(H) # "@var{H} a unique identifier of a database
   session connection.".

dbconnection( dbconnection(DbStream,DbHandle) ) :- 
	stream(DbStream),
	dbhandle(DbHandle).

:- comment(dbconnection/1,"@includedef{dbconnection/1}").

:- comment(doinclude,dbhandle/1).

:- regtype dbhandle(H) # "@var{H} is the internal database identifier
   (handle) of a database session.".

dbhandle( H ) :-
	string(H).

:- comment(dbhandle/1,"@includedef{dbhandle/1}").

:- regtype dbqueryconnection(H) # "@var{H} is a unique identifier of a
   query answer in a database session connection.".

dbqueryconnection(dbqueryconnection(DbConnection,StmtHandle)) :-
	dbconnection(DbConnection),
	stmthandle(StmtHandle).

:- comment(dbqueryconnection/1,"@includedef{dbqueryconnection/1}").

:- comment(stmthandle/1,"@includedef{stmthandle/1}").

:- regtype stmthandle(H) # "@var{H} is the internal statement identifier
   (handle) of a query answer in a database session connection.".

stmthandle(H) :-
	string(H).

:- comment(doinclude,stmthandle/1).

% ----------------------------------------------------------------------------

:- pred odbc_query(+DbConnection,+Sentence,-AnswerTableTerm) 
   :: dbconnection * sqlstring * answertableterm

# "Evaluates the SQL code in @var{Sentence} in database session
   @var{DbConnection}.  @var{AnswerTableTerm} is the response. If a wrong
   answer is obtained, @var{AnswerTableTerm} will indicate the error (see
   @decl{answertableterm} for details).".

odbc_query(dbconnection(Stream,DbHandle),Sentence,AnswerTerm) :-
	list_concat(["DirectSQL ",DbHandle,Sentence],ServerSentence),
	null_ended_string(ServerSentence,SentenceToSend),
	debug_message("Sending sentence: ~s",[SentenceToSend]),
	socket_send(Stream,SentenceToSend),
	debug_message("Waiting for response...",[]),
	socket_recv_long(Stream,Ans),
	debug_message("Unprocessed response is: ~s",[Ans]),
	null_ended_to_blank_ended(Ans,Answer),
	debug_message("Response is: ~s",[Answer]),
	process_eval_answer(Answer,AnswerTerm).

process_eval_answer(Answer,'ok') :-
	match_string("ok",Answer,_Rest), 
	!,
	debug_message("ok received",[]). %% ok received on successful insertions/deletions
process_eval_answer(Answer,AnswerTableTerm) :-
	match_string("table(",Answer,Rest),
	!,
	string2term("t("||Rest,AnswerTableTerm),
	debug_message("Converted to term: ~q",[AnswerTableTerm]).
%%%%%% TO MAKE: Need to fix type meta_predicate bug.
%	,answertableterm(AnswerTableTerm)

process_eval_answer(Answer,err(AnswerArgument)) :-
	atom_codes(ReceivedAnswerTerm,Answer),
	atom_concat('in DB answer, received ',ReceivedAnswerTerm,AnswerArgument),
	debug_message("Error returned in answer argument",[]).
%	error_message("in DB answer, received ""~s""",[Answer]),
%	fail.

match_string([],Rest,Rest).
match_string([H|T1],[H|T2],Rest) :-
	prefix(T1,T2,Rest).
match_string(S,[_H|T],Rest) :-
	match_string(S,T,Rest).

prefix([],Rest,Rest).
prefix([H|T1],[H|T2],Rest) :-
	prefix(T1,T2,Rest).

% ----------------------------------------------------------------------------

:- pred odbc_free_query_connection(+DbQueryConnection) 
   :: dbqueryconnection

# "Frees a SQL query connection."

odbc_free_query_connection(_DbQueryConnection).

% ----------------------------------------------------------------------------

:- pred odbc_query_one_tuple(+DbConnection,+Sentence,-DbQueryConnection) 
   :: dbconnection * sqlstring * dbqueryconnection

# "Evaluates the SQL code in @var{Sentence} in database session
   @var{DbConnection}.  If @var{Sentence} is a succesfull selection,
   @var{DbQueryConnection} is a @em{handle} to the set of tuples produced
   by this selection. The individual members of this set can then be
   accessed via @var{DbQueryConnection} using the @pred{odbc_fetch/2}
   predicate. If @var{Sentence} is not succesfull or is not a selection,
   @var{DbQueryConnection} will contain the answer received.".

odbc_query_one_tuple(DbConnection,Sentence,dbqueryconnection(DbConnection,StmtHandleOrOtherAnswer)):- 
	odbc_query_one_tuple_2(DbConnection,Sentence,StmtHandleOrOtherAnswer),
 %%%%%% TO MAKE: checking 'Answer' we have one call less
	process_odbc_stmt_answer(StmtHandleOrOtherAnswer).

% TODO: Handle with exceptions!!!
process_odbc_stmt_answer(Answer):-
	debug_message("Processing stmthandle ~s",[Answer]),
	match_string("ERROR",Answer,_Rest),
	debug_message("ERROR has been matched",[]),
	!,
	error_message("~s",[Answer]),
	fail.
process_odbc_stmt_answer(_Answer).

odbc_query_one_tuple_2(DbConnection,Sentence,StmtHandleOrOtherAnswer):- 
        DbConnection = dbconnection(Stream,DbHandle),
	list_concat(["SQL ",DbHandle," ",Sentence],ServerSentence),
	null_ended_string(ServerSentence,SentenceToSend),
	debug_message("Sending sentence: ~s",[SentenceToSend]),
	socket_send(Stream,SentenceToSend),
	debug_message("Waiting for response...",[]),
	socket_recv_long(Stream,StmtHand),
	debug_message("StmtHandle received is: ~s",[StmtHand]),
	null_ended_to_blank_ended(StmtHand,StmtHandle),
	debug_message("Response is: ~s",[StmtHandle]),
	!,
	process_stmt_answer(StmtHandle,StmtHandleOrOtherAnswer),
	debug_message("Stmt answer processed is ~s",
	              [StmtHandleOrOtherAnswer]).

odbc_query_one_tuple_2(_,Sentence,Answer):- 
	append("ERROR in answer for ",Sentence,Answer),
	debug_message("Error returned in answer argument",[]).
%	error_message("in answer for ~s",[Sentence]),
%	fail.

process_stmt_answer(Ok,Ok):-
	match_string("ok",Ok,_Rest),
	!,
	debug_message("ok received",[]). 
        %% ok received on successful insertions/deletions
process_stmt_answer(StmtHandle,StmtHandle):-
	match_string("StmtHandle",StmtHandle,_Rest),
	!,
	debug_message("StmtHandle recognized",[]).
process_stmt_answer(Error,Answer):-
	append("ERROR in DB answer, received ",Error,Answer),
	debug_message("Error to return is ~s",[Answer]),
	debug_message("stmt_answer processed",[]).
%	error_message("in DB answer, received ""~s""",[Answer]),
%	fail.

% ----------------------------------------------------------------------------
:- pred odbc_fetch(+DbQueryConnection,-TupleTerm)
   :: dbqueryconnection * answertupleterm

# "@var{TupleTerm} represents the last tuple read from handle
   @var{DbQueryConnection}. Will be an empty list if all the answers have
   already been read. It displays an error if any unformatted string is fetched.".

odbc_fetch(dbqueryconnection(DbConnection,StmtHandle),TupleTerm):-
        DbConnection = dbconnection(Stream,DbHandle),
	list_concat(["fetch ",DbHandle," ",StmtHandle],ServerSentence),
	null_ended_string(ServerSentence,SentenceToSend),
	debug_message("Sending sentence: ~s",[SentenceToSend]),
	socket_send(Stream,SentenceToSend),
	debug_message("Waiting for response...",[]),
	socket_recv_long(Stream,Ans),
	debug_message("Unprocessed response is: ~s",[Ans]),

	debug_message("Going with second step",[]),


	null_ended_to_blank_ended(Ans,Answer),

%%%%%%%%%%%%%%%%%%%%% REMOVE 
%%	socket_recv(Stream,Ans2),
%%	debug_message("Unprocessed response is: ~s",[Ans2]),
%%	null_ended_to_blank_ended(Ans2,Answer2),
%%	debug_message("Response 2 is: ~s",[Answer2]),
%%%%%%%%%%%%%%%%%%%%%

	debug_message("Response is: ~s",[Answer]),
	( match_string("Fetch : at eof",Answer,_Rest) ->
            TupleTerm = []
        ; TupleTerm = tup(Tuple),
          append("["||Answer,"]",BrackStr),
          string2term(BrackStr,Tuple)
        ),
	!.

% ----------------------------------------------------------------------------

:- pred odbc_disconnect(+DbConnection) :: dbconnection

# "Logs off from the database identified by @var{DbConnection}.
   It fails and display an error message if the login is not succesfully completed.".

odbc_disconnect(DbConnection):- 
	DbConnection = dbconnection(Stream,_DbHandle),
	odbc_disconnect_2(DbConnection),
	close(Stream),
	debug_message("Disconnected from stream",[]).

odbc_disconnect_2(dbconnection(Stream,DbHandle)):- 
	append("logoff ",DbHandle,SentenceToLogoffDb),
	null_ended_string(SentenceToLogoffDb,SentenceToSend),
	debug_message("Logging off by sending: ~s",[SentenceToSend]),
	socket_send(Stream,SentenceToSend),
	debug_message("Waiting for response...",[]),
	socket_recv_long(Stream,Resp),
	null_ended_to_blank_ended(Resp,Response),
	debug_message("Response is: ~s",[Response]),
	process_logoff_answer(Response),
	!,
	retract_fact(session(DbHandle,_DbName,_User)).
odbc_disconnect_2(dbconnection(_Stream,DbHandle)) :- 
	retract_fact(session(DbHandle,DbName,User)),
	!,
	error_message("logoff not completed for ~w@~w: ~s",[User,DbName,DbHandle]),
	fail.
odbc_disconnect_2(DbConnection) :- 
	!,
	error_message("logoff not completed for ~w",[DbConnection]),
	fail.

process_logoff_answer(Answer):-
	match_string("ok",Answer,_Rest),
	!,
	debug_message("ok received.",[]).
process_logoff_answer(Answer):-
	error_message("logoff error: ~s",[Answer]),
	fail.


% ----------------------------------------------------------------------------

last_block(String):-
	null_ended_string(_StrWithoutNull,String).

socket_recv_long(Stream,Buf):-
	socket_recv_code(Stream,String1,Code),
	debug_message(" Received ~s. Code is ~w",[String1,Code]),
	(last_block(String1)
	  ->
	   debug_message(" Last block has been received",[]),
	   append(String1,"",Buf),
	   debug_message(" Buffer received is ~s",[Buf])
	;
	   socket_recv_long(Stream,String2),
	   debug_message(" Second part of buffer received is ~s",[String2]),
	   append(String1,String2,Buf),
	   debug_message(" Buffer received is ~s",[Buf])
	).
	   
	
:- pred odbc_get_tables(+DbConnection,-Tables) :: dbconnection * list(atm)

# "@var{Tables} contains the tables available in @var{DbConnection}.".

odbc_get_tables(dbconnection(Stream,DbHandle),List):-
	append("DatabaseTables ",DbHandle,SentenceToGetTablesDb),
	null_ended_string(SentenceToGetTablesDb,SentenceToSend),
	debug_message(" Sending: ~s",[SentenceToSend]),
	socket_send(Stream,SentenceToSend),
	debug_message("Waiting for response...",[]),
	socket_recv_long(Stream,Resp),
	null_ended_to_blank_ended(Resp,Response),
	debug_message("Response is: ~s",[Response]),
	string2term(Response,List),
	process_get_tables_answer(List),
	!.

odbc_get_tables(Conn,Answer):-
	!, 
	error_message("could not get tables list for ~w, answer received is ~w",[Conn,Answer]),
	fail.

process_get_tables_answer(Answer):-
	list(Answer,atom),
	!. 
process_get_tables_answer(_Answer):-
	error_message("answer received is not a tables list",[]),
	fail.


:- pred odbc_table_types(+DbConnection,+Table,-AttrTypes) 
	:: dbconnection * atm * list

# "@var{AttrTypes} are the attributes and types of @var{Table} in
  @var{DbConnection}.".

odbc_table_types(dbconnection(Stream,DbHandle),TableName,AttTypesList):-
	atom_codes(TableName,TableNameStr),
	list_concat(["TableAttributes ",DbHandle," ",TableNameStr],
                    SentenceToGetTableTypes),
	null_ended_string(SentenceToGetTableTypes,SentenceToSend),
	debug_message(" Sending: ~s",[SentenceToSend]),
	socket_send(Stream,SentenceToSend),
	debug_message("Waiting for response...",[]),
	socket_recv_long(Stream,Resp),
	null_ended_to_blank_ended(Resp,Response),
	debug_message("Response is: ~s",[Response]),
	string2term(Response,AttTypesList),
	list(AttTypesList),
	!.
odbc_table_types(Conn,Answer,_):-
	error_message("could not get table attributes for ~w, answer received is ~w",[Conn,Answer]),
	fail.

% ----------------------------------------------------------------------------
% Support predicates:
% ----------------------------------------------------------------------------

null_ended_string([], [0]).
null_ended_string([X|Xs], [X|NXs]):-
        null_ended_string(Xs, NXs).

null_ended_to_blank_ended([0]," "):-
	!.
null_ended_to_blank_ended([First|Rest1],[First|Rest2]):-
	null_ended_to_blank_ended(Rest1,Rest2).

%%%%%% TO MAKE: see if 'dbfree_stmt_handle' would be useful. 
%% If useful, then dbqueryconnection storage will be implemented: 
%% when 'odbc_disconnect(DbHandleN)' called, all StmtHandlei in DbHandleN will 
%% be disposed. 'Free DbHandleN StmHandlei' will be sent to the server.

% ----------------------------------------------------------------------------

:- use_module(library(lists), [length/2]).

string2term(String,Term):-
	parse_term(String,Term, _).

parse_term([],'',[]).
parse_term([C|String0],Term,String):-
	parse_term0(C,String0,Term,String).
	
parse_term0(0'\s,String0,Term,String):- % space
        parse_term(String0,Term,String).
parse_term0(0'[,String0,Term,String):- !,
	parse_args0(String0,Term,[0']|String]).
parse_term0(0'( , _, _, _):- !, fail.
parse_term0(0') , _, _, _):- !, fail.
parse_term0(0'] , _, _, _):- !, fail.
parse_term0(0', , _, _, _):- !, fail.
parse_term0(C0,String0,Term,String):-
        parse_functor(C0,String0,FunctorStr,String1),
        ( length(FunctorStr, L), L > 500 ->
            Term = FunctorStr,
            String = String1
        ;
            name(Functor,FunctorStr),
            parse_args(String1,Args,String),
            Term=..[Functor|Args]
        ).

parse_functor(0'\s,String,[],[0'\s|String]).
parse_functor(0'( ,String,[],[0'( |String]).
parse_functor(0') ,String,[],[0') |String]).
parse_functor(0'[ ,String,[],[0'[ |String]).
parse_functor(0'] ,String,[],[0'] |String]).
parse_functor(0', ,String,[],[0', |String]).
parse_functor(0'' ,[C|String0],Functor,String):- !,
	parse_quoted(C,String0,Functor,String).
parse_functor(C, String,[C|Functor],String1):-
        parse_functor_(String,Functor,String1).

parse_functor_([], [], []).
parse_functor_([C|String],Functor,String1):-
	parse_functor(C,String,Functor,String1).

%parse_args([],[],[]).
parse_args([0'(|String0],Args,String):- !,
	parse_args0(String0,Args,[0')|String]).
parse_args(String,[],String).

parse_args0(String0,[Arg|Args],String):-
	parse_term(String0,Arg,String1), !,
	parse_args1(String1,Args,String).
parse_args0(String, [], String).

parse_args1([0'\s|String0],Args,String) :- !,
        parse_args1(String0,Args,String).
parse_args1([0', |String0],[Arg|Args],String):- !,
	parse_term(String0,Arg,String1),
	parse_args1(String1,Args,String).
parse_args1(String,[],String).

parse_quoted(0'',[0'',C|String0],[0''|Functor],String) :- !,
        parse_quoted(C,String0,Functor,String).
parse_quoted(0'',String,[],String) :- !.
parse_quoted(C1, [C|String0],[C1|Functor],String) :-
        parse_quoted(C,String0,Functor,String).

% changelog:
%   db replaced by odbc.
%   db_connect replaces odbc_connect and db_login.
%   db_disconnect replaces odbc_disconnect and db_logoff.
%   changed dbqueryconnect.
