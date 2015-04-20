:- module(database_client,
          [connect/1,login_db/3,sql_sentence/4,disconnect/1,sql_fast/3]).

:- include(library(assertions)).
:- include(library(types)).
:- include(library(basicmodes)).
:- use_module(library(basicprops)).

:- use_module(library(sockets),
	[connect_to_socket/3, socket_recv/2, socket_send/2]).
:- use_module(library(write),[write/1]).
:- use_module(library(strings),[get_line/1, write_string/1]).
:- use_module('string2term',[string2term/2]). 
:- use_module(library(lists),[append/3]).

:- comment(title,"Low Level Socket Interface to SQL/ODBC Databases").

:- comment(author,"D. Cabeza and M. Carro, following the interface
   specification developed by C. Taboch").

sql_db_id("SQL Anywhere 5.0 Sample"). %%% WARNING: WAS A STRING  MH
sql_db_passwd('').                    %%% WARNING: NEW  MH
sql_server_name('r2d5.dia.fi.upm.es').
sql_server_port(2020).

%% Example of use:
%%  connect(Stream), 
%%  login_db(Stream,"SQL Anywhere 5.0 Sample",DbHandle),
%%  sql_sentence(Stream,DbHandle,"SELECT fname,lname,address from ""DBA"".customer WHERE ((Id>100) AND (Id<105))",Term), 
%%  sql_sentence(Stream,DbHandle,"SELECT id,name,description,color from ""DBA"".product WHERE size='One size fits all' OR size='Large'",Term2), 
%%  disconnect(Stream).
        
sql_fast(DBId,SQLString,ResultTerm):-
	connect(Stream),
	sql_db_id(Id),
	login_db(Stream,Id,DbHandle),
	sql_sentence(Stream,DbHandle,SQLString,ResultTerm),
	disconnect(Stream).

connect(Stream):-
        sql_server_name(Server),
        sql_server_port(Port),
	connect_to_socket(Server, Port, Stream),
	write('Connected to stream \n').

disconnect(Stream):-
	close(Stream),
	write('Disconnected stream').
        

login_db(Stream,DbNameString,DbHandle):- 
%%DbHandle is a string to be used to dialog with the Server Application
	append("logon ",DbNameString,SentenceToLoginDb),
	null_ended_string(SentenceToLoginDb,SentenceToSend),
	write_string(SentenceToSend),
	socket_send(Stream,SentenceToSend),
	write('Login sent \n'),
	socket_recv(Stream,DbHand),
	null_ended_to_blank_ended(DbHand,DbHandle),
	write('DbHandle received is: '),
	write_string(DbHandle),
	nl.

	

sql_sentence(Stream,DbHandle,Sentence,ResultTerm):-
	append("SQL ",DbHandle,SentencePreffix),
	append(SentencePreffix,Sentence,ServerSentence),
	null_ended_string(ServerSentence,SentenceToSend),
	socket_send(Stream,SentenceToSend),
	write('Sentence sent is: '),
	write_string(SentenceToSend),
	socket_recv(Stream,Answer),
	write('\nAnswer received is:'),
	write_string(Answer),
	nl,
	string2term(Answer,ResultTerm),
	write('\n Converted to term: '),
	write(ResultTerm).
	
null_ended_string([], [0]).
null_ended_string([X|Xs], [X|NXs]):-
        null_ended_string(Xs, NXs).

blank(C):-        " "=[C].

null_ended_to_blank_ended([0],[C]):-
	blank(C),!.
null_ended_to_blank_ended([First|Rest1],[First|Rest2]):-
	null_ended_to_blank_ended(Rest1,Rest2).
