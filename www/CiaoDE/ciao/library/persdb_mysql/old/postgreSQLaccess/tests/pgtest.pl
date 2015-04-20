:- use_module(library('persdb_sql/postgreSQLaccess/pg_client')).
:- use_module(library('persdb_sql/postgreSQLaccess/pg_packets')).
:- use_module(library(lists)). 
:- use_module(library(sockets)).
:- use_module(library(messages)).
:- use_module(library(format)).
:- use_module(library(system)).

 :- multifile issue_debug_messages/1.
 :- data issue_debug_messages/1.
 issue_debug_messages('pg_packets').
 issue_debug_messages('pg_client').
 issue_debug_messages('pgtest').

%send_startup_packet(Stream):-
%	connect_to_socket('localhost',2020,Stream),
%	startup_packet("template1","ignacio",F),
%	length(F,Length),
%	format("Sending packet, ~w bytes...",[Length]),nl,
%	format("Packet to send is ""~s"".",[F]),nl,
%	socket_send(Stream,F),
%	format("Waiting for response...",[]).

%receive_startup_answer(Stream):-
%	format("Receiving answer...",[]),
%	socket_recv(Stream,Answer),
%	format("Answer received is ~s ...",[Answer]).
	
%test_startup:-
%	format("Testing startup ...",[]),
%	send_startup_packet(Stream),
%	receive_startup_answer(Stream),
%	close(Stream).


%init_pgserver :-
%	current_fact(pgserver_started),
%	!.
%init_pgserver :-
%	system('postmaster -D /home/ignacio/postgreDBtest -i -p 2020 & ',0),
%	!,
%	assertz_fact(pgserver_started).
%init_pgserver :-
%	warning("Server couldn't be started. Is the port being used yet ... ?").
 
test :-
	%% init_pgserver,
	db_connect(localhost:2020, Stream),
	pg_login(Stream, template1, ignacio, ignacio, dbconnection(Stream,_PID,_SecretKey)),
	query_packet("SELECT * FROM AUTHORS",FrontEndPacket),
	socket_send(Stream,FrontEndPacket),
	debug_message("Query has been sent"),
	socket_recv(Stream,Answer),
	format("Answer received is ~s\n",[Answer]),
	format("Answer received in raw format is ~w\n",[Answer]),
        db_disconnect(Stream).
test00 :-
	%% init_pgserver,
	db_connect(localhost:2020, Stream),
	pg_login(Stream, template1, ignacio, ignacio, dbconnection(Stream,_PID,_SecretKey)),
	query_packet("SELECT * FROM AUTHORS where lastname='Pedrin'",FrontEndPacket),
	socket_send(Stream,FrontEndPacket),
	debug_message("Query has been sent"),
	socket_recv(Stream,Answer),
	format("Answer received is ~s\n",[Answer]),
	format("Answer received in raw format is ~w\n",[Answer]),
        db_disconnect(Stream).
 
test01 :-
	%% init_pgserver,
	db_connect(localhost:2020, Stream),
	pg_login(Stream, template1, ignacio, ignacio, dbconnection(Stream,_PID,_SecretKey)),
	query_packet("SELECT typname,oid FROM pg_type",FrontEndPacket),
	socket_send(Stream,FrontEndPacket),
	debug_message("Query has been sent"),
	socket_recv(Stream,Answer),
	format("Answer received is ~s\n",[Answer]),
%	format("Answer received in raw format is ~w\n",[Answer]),
        db_disconnect(Stream).

test2 :-
	%% init_pgserver,
	db_connect('clip.dia.fi.upm.es':2020, Stream),
	pg_login(Stream, template1, ignacio, ignacio, dbconnection(Stream,PID,SecretKey)),
	pg_eval_sql(dbconnection(Stream,PID,SecretKey),"SELECT * FROM AUTHORS",AnswerTerm),
	format("Answer received is ~w",[AnswerTerm]),
        db_disconnect(Stream).


test03(First,Rest) :-
	%% init_pgserver,
	first_null_ended_string(
				   First,
				   Rest,
				   [80,98,108,97,110,107,0,84,0,3,108,97,115,116,110,97,109,101,0,0,0,4,19,255,255,102,105,114,115,116,110,97,109,101,0,0,0,4,19,255,255,99,111,100,101,0,0,0,0,21,0,2,68,224,0,0,0,14,68,101,32,108,97,32,86,101,103,97,0,0,0,13,71,97,114,99,105,108,97,115,111,0,0,0,5,49,68,224,0,0,0,21,86,97,122,113,117,101,122,32,77,111,110,116,97,108,98,97,110,0,0,0,10,77,97,110,117,101,108,0,0,0,5,50,68,224,0,0,0,17,80,101,114,101,122,45,82,101,118,101,114,116,101,0,0,0,10,65,114,116,117,114,111,0,0,0,5,51,67,83,69,76,69,67,84,0]),
	debug_message("Before first null: ~w, After first null: ~w",[First,Rest]).


test04(First,Rest) :-
	%% init_pgserver,
	first_null_ended_string(
				   First,
				   Rest,
				   "Answer received is Pblank
			       ).

test05(List,AfterPacket) :-
	%% init_pgserver,
	fieldsDescrList2String(List,3,"lastname

% TO MAKE : test it
% rowDescription_packet(2,List,"T
% fieldsDescrList2String(List,3,"lastname
% fieldsDescrList2String([["lastname",3,12],["firstname",4,22]],2,Pack,After).

% fieldsDescrList2String(List,1,"lastname
