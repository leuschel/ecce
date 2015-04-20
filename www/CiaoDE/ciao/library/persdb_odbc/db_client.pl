:- module(db_client,
	[
	    odbc_connect/2,
	    db_login/5,
 	       socketname/1,
	       dbname/1,
	       user/1,
	       passwd/1,
	       dbconnection/1,
	    db_eval_sql/3,
	       answertableterm/1,
	       tuple/1,
	    db_stmt_handle/3,
	       dbqueryconnection/1,
	    db_one_tuple/2,
	       answertupleterm/1,
	    db_get_tables/2,
	    db_table_types/3,
	    db_logoff/1,
	    odbc_disconnect/1,
	    match_string/3 %% for persdb_sql use. TO MAKE: put in other library
	],[assertions,regtypes,basicmodes]).

:- use_module(library(sockets),
     [connect_to_socket/3, socket_recv/2, socket_recv_code/3, socket_send/2]).

:- use_module(library(write),[write/1]).
:- use_module(library(strings),[get_line/1, write_string/1]).
:- use_module(library(lists),[append/3, list_concat/2]).
%% ,list/1,list/2]).

:- use_module(library('persdb_odbc/string2term'),[string2term/2]). 

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

:- pred odbc_connect(+DbAddress,-Stream) :: socketname * stream 

# "Opens a socket connection to port @var{DbAddress}, which should be
  the address of a @concept{database mediator server}. @var{Stream} is
  the identifier of the corresponding Prolog stream. It is simply a
  call to @pred{connect_to_socket/3}".

odbc_connect(Server:Port,Stream):-
	debug_message("Connecting to server ~w at port ~w",[Server,Port]), 
	connect_to_socket(Server, Port, Stream).

:- regtype socketname(IPP) # "@var{IPP} is a structure describing a complete TCP/IP port address.".

socketname( IPAddress : PortNumber ) :- 
 	atm(IPAddress),
 	int(PortNumber).

:- comment(socketname/1,"@includedef{socketname/1}").

:- pred odbc_disconnect(+Stream) :: stream

# "Closes the socket connection to @var{Stream}. It is simply a call
   to @pred{close/1}.".

odbc_disconnect(Stream) :-
	close(Stream),
	debug_message("Disconnected from stream",[]).
        
% ----------------------------------------------------------------------------

:- pred db_login(+Stream,+DbName,+User,+Passwd,-DbConnection) 
   :: stream * dbname * user * passwd * dbconnection

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
	
db_login(Stream,DbName,User,Passwd,dbconnection(Stream,DbHandle)):-
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
	
db_login(Stream,DbName,User,Passwd,dbconnection(Stream,DbHandle)):- 
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

db_login(Stream,_DbName,User,Passwd,dbconnection(Stream,_DbHandle)):- 
	User=='',Passwd=='',
	\+login_without_userid_permitted,
	!,
	error_message("login without user identifier not permitted",[]).

db_login(_Stream,DbName,User,_Passwd,_) :- 
	error_message("login not completed for ~w@~w:",[User,DbName]),
	fail.

:- data session/3.

:- pred session(DbHandle,DbName,User) :: dbhandle * dbname * user 

# "Stores session info by relating @var{DbHandle} with @var{DbName}
  and @var{User}.".

% ----------------------------------------------------------------------------

:- regtype dbname(DBId) # "@var{DBId} is the identifier of an ODBC database.".

dbname(DBId) :- 
	atm(DBId).

:- comment(dbname/1,"@includedef{dbname/1}").

:- regtype user(User) # "@var{User} is a user name in the ODBC database.".

user(User) :- 
	atm(User).

:- comment(user/1,"@includedef{user/1}").

:- regtype passwd(Passwd) # "@var{Passwd} is the password for the user
   name in the ODBC database.".

passwd(Passwd) :- 
	atm(Passwd).

:- comment(passwd/1,"@includedef{passwd/1}").

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

dbqueryconnection(dbqueryconnection(DbStream,DbHandle,StmtHandle)) :-
	stream(DbStream),
	dbhandle(DbHandle),
	stmthandle(StmtHandle).

:- comment(dbqueryconnection/1,"@includedef{dbqueryconnection/1}").

:- comment(stmthandle/1,"@includedef{stmthandle/1}").

:- regtype stmthandle(H) # "@var{H} is the internal statement identifier
   (handle) of a query answer in a database session connection.".

stmthandle(H) :-
	string(H).

:- comment(doinclude,stmthandle/1).

% ----------------------------------------------------------------------------

:- pred db_eval_sql(+DbConnection,+Sentence,-AnswerTableTerm) 
   :: dbconnection * sqlstring * answertableterm

# "Evaluates the SQL code in @var{Sentence} in database session
   @var{DbConnection}.  @var{AnswerTableTerm} is the response. If a wrong
   answer is obtained, @var{AnswerTableTerm} will indicate the error (see
   @decl{answertableterm} for details).".

db_eval_sql(dbconnection(Stream,DbHandle),Sentence,AnswerTerm) :-
	list_concat(["DirectSQL ",DbHandle,Sentence],ServerSentence),
	null_ended_string(ServerSentence,SentenceToSend),
	debug_message("Sending sentence: ~s",[SentenceToSend]),
	socket_send(Stream,SentenceToSend),
	debug_message("Waiting for response...",[]),
	socket_recv_long(Stream,Ans),
	debug_message("Unprocessed response is: ~s",[Ans]),
	null_ended_to_blank_ended(Ans,Answer),
	debug_message("Response is: ~s",[Answer]),
	process_eval_answer(Answer,AnswerTerm),
	!.
db_eval_sql(_,Sentence,err(AnswerArgument)) :- 
	atom_codes(SentTerm,Sentence),
	atom_concat('in answer to ',SentTerm,AnswerArgument).
%	error_message("in answer for ~s",[Sentence]),
%	fail.

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

:- pred db_stmt_handle(+DbConnection,+Sentence,-DbQueryConnection) 
   :: dbconnection * sqlstring * dbqueryconnection

# "Evaluates the SQL code in @var{Sentence} in database session
   @var{DbConnection}.  If @var{Sentence} is a succesfull selection,
   @var{DbQueryConnection} is a @em{handle} to the set of tuples produced
   by this selection. The individual members of this set can then be
   accessed via @var{DbQueryConnection} using the @pred{db_one_tuple/2}
   predicate. If @var{Sentence} is not succesfull or is not a selection,
   @var{DbQueryConnection} will contain the answer received.".

db_stmt_handle(dbconnection(Stream,DbHandle),Sentence,
               dbqueryconnection(Stream,DbHandle,StmtHandle_or_Other_Answer)):-
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
	process_stmt_answer(StmtHandle,StmtHandle_or_Other_Answer),
	debug_message("Stmt answer processed is ~s",
	              [StmtHandle_or_Other_Answer]).
db_stmt_handle(_,Sentence,dbqueryconnection(_Stream,_DbHandle,Answer)):- 
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
:- pred db_one_tuple(+DbQueryConnection,-TupleTerm)
   :: dbqueryconnection * answertupleterm

# "@var{TupleTerm} represents the last tuple read from handle
   @var{DbQueryConnection}. Will be an empty list if all the answers have
   already been read. It displays an error if any unformatted string is fetched.".

db_one_tuple(dbqueryconnection(Stream,DbHandle,StmtHandle),TupleTerm):-
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

:- regtype answertupleterm(X) #  "@var{X} is a predicate containing a tuple.".

answertupleterm([]).
answertupleterm(tup(T)) :-
	tuple(T).

:- comment(answertupleterm/1,"@includedef{answertupleterm/1}").

% ----------------------------------------------------------------------------

:- regtype sqlstring(S) # "@var{S} is a string of SQL code.".

sqlstring( S ) :- 
	string(S).

:- comment(sqlstring/1,"@includedef{sqlstring/1}").

:- comment(bug,"Need to fix the problem with parametric types being
   meta_predicates ... list(_,tuple/1).").

:- comment(answertableterm/1,"Represents the types of responses that
   will be returned from the ODBC database interface. These can be a
   set of answer tuples, or the atom @tt{ok} in case of a successful
   addition or deletion.").

:- regtype answertableterm(AT) # "@var{AT} is a response from the ODBC
   database interface.".

answertableterm(ok).
answertableterm(t(Answers)) :-
	list(Answers,tuple).
answertableterm(err(Answer)) :-
	term(Answer).	

:- comment(answertableterm/1,"@includedef{answertableterm/1}").

:- regtype tuple(T) # "@var{T} is a tuple of values from the ODBC database
   interface.".

tuple(T) :-
	list(T,atm).

:- comment(tuple/1,"@includedef{tuple/1}").

% ----------------------------------------------------------------------------

:- pred db_logoff(+DbConnection) :: dbconnection

# "Logs off from the database identified by @var{DbConnection}.
   It fails and display an error message if the login is not succesfully completed.".

db_logoff(dbconnection(Stream,DbHandle)):- 
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
db_logoff(dbconnection(_Stream,DbHandle)) :- 
	retract_fact(session(DbHandle,DbName,User)),
	!,
	error_message("logoff not completed for ~w@~w: ~s",[User,DbName,DbHandle]),
	fail.
db_logoff(Conn) :- 
	!,
	error_message("logoff not completed for ~w",[Conn]),
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
	   
	
:- pred db_get_tables(+DbConnection,-Tables) :: dbconnection * list(atm)

# "@var{Tables} contains the tables available in @var{DbConnection}.".

db_get_tables(dbconnection(Stream,DbHandle),List):-
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

db_get_tables(Conn,Answer):-
	!, 
	error_message("could not get tables list for ~w, answer received is ~w",[Conn,Answer]),
	fail.

process_get_tables_answer(Answer):-
	list(Answer,atom),
	!. 
process_get_tables_answer(_Answer):-
	error_message("answer received is not a tables list",[]),
	fail.


:- pred db_table_types(+DbConnection,+Table,-AttrTypes) 
	:: dbconnection * atm * list

# "@var{AttrTypes} are the attributes and types of @var{Table} in
  @var{DbConnection}.".

db_table_types(dbconnection(Stream,DbHandle),TableName,AttTypesList):-
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
db_table_types(Conn,Answer,_):-
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
%% when 'db_logoff(DbHandleN)' called, all StmtHandlei in DbHandleN will 
%% be disposed. 'Free DbHandleN StmHandlei' will be sent to the server.
