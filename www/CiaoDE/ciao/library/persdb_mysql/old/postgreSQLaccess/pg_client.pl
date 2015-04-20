:- module(pg_client,
	[
	    db_connect/2,
	    pg_login/5,
	    pg_eval_sql/3,
	    db_disconnect/1
	],
	[assertions]).

:- use_module(library('persdb_sql/postgreSQLaccess/pg_packets')).
:- use_module(library(sockets)).
:- use_module(library(messages)).
:- use_module(library(lists)).

%% TO SEE : it has sense to split db_connect and pg_login into two operations
db_connect(Server:Port,Stream) :-
	connect_to_socket(Server,Port,Stream).
	
%% IMPORTANT, TO SEE: Accept strings or atoms as 'pg_login' arguments? Or both? (type detection and conversion)
%% TO SEE : is a 'DbHandle' needed?
pg_login(Stream,DbNameAtom,UserAtom,PasswordAtom,dbconnection(Stream,BackEndPID,BackEndKey)) :-
	atom_codes(DbNameAtom,DbName),
	atom_codes(UserAtom,User),
	atom_codes(PasswordAtom,Password),
	startup_packet(DbName,User,FrontEndPacket),
	socket_send(Stream,FrontEndPacket),
	socket_recv(Stream,AuthenticationResponse),
	authentication_Ok(AuthenticationResponse,Password,dbconnection(Stream,BackEndPID,BackEndKey)),
	!,
	debug_message("Successful authentication",[]).
%% This lines must be added to implement 2.0 Frontend/Backend protocol
%	socket_recv(Stream,BackEndPacket), 
%	debug_message("BackEndPacket is ~w",[BackEndPacket]),
%	backendKeyData_Ok(BackEndPID, BackEndKey, BackEndPacket,dbconnection(Stream,BackEndPID,BackEndKey)).
	
pg_login(_,DbNameAtom,UserAtom,_,_) :-
	error_message("Couldn't logon user ~w to database ~w",[UserAtom, DbNameAtom]),
        fail.

backendKeyData_Ok(BackEndPID, BackEndKey, BackEndPacket, dbconnection(Stream,_,_)) :-
	backendKeyData_packet(BackEndPID, BackEndKey, BackEndPacket),
	!.

backendKeyData_Ok(_BackEndPID, _BackEndKey, BackEndPacket, dbconnection(Stream,_,_)) :-
	errorResponse_packet(ErrorMessage, BackEndPacket),
	!,
	error_message("postgres backend message (backend startup failed): \n~s",[ErrorMessage]),
	fail.
% The connection is closed after sending this message

backendKeyData_Ok(_BackEndPID, _BackEndKey, BackEndPacket, dbconnection(Stream,_,_)) :-
	readyForQuery_packet(BackEndPacket),
	!,
	error_message("Backend key data not received. Unexpected 'ReadyForQuery' packet",[]),
	fail.

backendKeyData_Ok(BackEndPID, BackEndKey, BackEndPacket, dbconnection(Stream,PID,Key)) :-
	noticeResponse_packet(NoticeMessage, BackEndPacket),
	!,
	warning_message("postgres backend message (waiting for backend key data): \n~s",[NoticeMessage]),
	socket_recv(Stream,NextPacket),
	backendKeyData_Ok(BackEndPID, BackEndKey, NextPacket, dbconnection(Stream,PID,Key)).

backendKeyData_Ok(_BackEndPID, _BackEndKey, _BackEndPacket, dbconnection(Stream, _PID, _Key)) :-
	error_message("Unexpected packet ('BackendKeyData' packet expected)"),
	fail.

%% TO SEE : if authentication is not succesful, is not better to close the socket? I will join db_connect and pg_login in a sole predicate.

:- discontiguous authentication_Ok/3.

authentication_Ok(AuthResponse,_Password,dbconnection(Stream,_,_)) :-
	errorResponse_packet(ErrorMessage,AuthResponse),
	!,
	error_message("postgres postmaster message (authentication):\n ~s",[ErrorMessage]),
	fail.
% TO SEE : The postmaster immediately closes the connection

% TO MAKE AND TO SEE : If the frontend does not support the authentication method requested by the postmaster, then it should immediately close the connection. 
authentication_Ok(AuthResponse,_Password,dbconnection(_Stream,_,_)) :-
	authenticationOk_packet(AuthResponse),
	!.
%% TO MAKE : send "SET datestyle + 'ISO'"

authentication_Ok(AuthResponse,Password,dbconnection(Stream,_,_)) :-
	authenticationUnencryptedPassword_packet(AuthResponse),
	!,
	unencryptedPassword_packet(Password,FrontEndPacket),
	socket_send(Stream,FrontEndPacket),
	socket_recv(Stream,PostMasterPacket),
	password_accepted(PostMasterPacket).

password_accepted(PostMasterPacket) :-
	authenticationOk_packet(PostMasterPacket),
	!.
password_accepted(PostMasterPacket) :-
	errorResponse_packet(ErrorMessage,PostMasterPacket),
	error_message("postgres postmaster message (checking the password):\n ~s",[ErrorMessage]),
	fail.

authentication_Ok(AuthResponse,_Password,dbconnection(Stream,_,_)) :-
	authenticationEncryptedPassword_packet(_Salt,AuthResponse),
	!,
	error_message("Crypt authentication not supported",[]),
	fail.
%% TO MAKE : find a way to access crypt()

authentication_Ok(AuthResponse,_Password,dbconnection(Stream,_,_)) :-
	authenticationKerberosV4_packet(AuthResponse),
	!,
	error_message("Kerberos v4 authentication not supported",[]),
	fail.
authentication_Ok(AuthResponse,_Password,dbconnection(Stream,_,_)) :-
	authenticationKerberosV5_packet(AuthResponse),
	!,
	error_message("Kerberos v5 authentication not supported",[]),
	fail.
authentication_Ok(AuthResponse,_Password,dbconnection(Stream,_,_)) :-
	authentication_packet(AuthResponse),
	!,
	error_message("Unknown authentication type : can't do it",[]),
	fail.
authentication_Ok(AuthResponse,_Password,dbconnection(Stream,_,_)) :-
	error_message("Expected an authentication response, received ~w",[AuthResponse]),
	fail.
%% TO SEE : is neccesary to handle NoticeResponse packets in the authentication phase?

db_disconnect(Stream) :-
	close(Stream).

%% TO SEE: replace RowsList by AnswerTerm, adding postgres2CIAOData(RowsList,AnswerTerm) predicate
pg_eval_sql(dbconnection(Stream,BackEndPID,BackEndKey),Sentence, RowsList):-
%% TO MODIFY: at the moment we only deal with select sentences (not insert, delete, queries ...). COMPLETE THIS.
	%% Supposing Sentence is a SELECT sentence
	query_packet(Sentence,FrontEndPacket),
	debug_message("Query packet is ~s",[FrontEndPacket]),
	socket_send(Stream,FrontEndPacket),
	socket_recv(Stream,BackEndPacket),
	debug_message("Packet received is ~s",[BackEndPacket]),
%% TO SEE: could be CopyOutResponse received?
	append(CursorPacket,FromRowDescrPacket,BackEndPacket),
	cursorResponse_Ok(CursorPacket,dbconnection(Stream,BackEndPID,BackEndKey)),
	debug_message("Cursor Response packet is ~s",[CursorPacket]),
	debug_message("Rest of packet is ~s",[FromRowDescrPacket]),
	rowDescription_packet(NumberOfFields,FieldsDescriptionList,FromRowDescrPacket,FromRowsDataPacket,RowDescrPacket),
	debug_message("RowDescription packet is ~s",[RowDescrPacket]),
	debug_message("FieldsDescriptionList is ~w",[FieldsDescriptionList]),

	rowsData_packet(NumberOfFields,FieldsDescriptionList,RowsList,FromRowsDataPacket,AfterRowsDataPacket,RowsDataPacket),

%%	asciiRow_packet(NumberOfFields,FieldsDescriptionList,BitMap,ValuesList,Fragment2,AfterRowPacket,RowsDataPacket),

%% TO MODIFY: at the moment we give as a response only a list of atoms' lists. When dealing with insertions, deletions, ... we will have other kind of answers (e.g. ok)
	debug_message("RowsData packet is ~w",[RowsDataPacket]),
	append(CompletedResponsePacket,[],AfterRowsDataPacket),
	completedResponse_packet("SELECT",CompletedResponsePacket),
	debug_message("Completed Response packet is ~s",[CompletedResponsePacket]).
%	list_concat([CursorPacket,RowDescrPacket,RowsDataPacket,CompletedResponsePacket],BackEndPacket).
	

pg_eval_sql(dbconnection(Stream,_BackEndPID,_BackEndKey), Sentence, _AnswerTerm):-
	error_message("SQL sentence could not be evaluated (~w)",[Sentence]),
	fail.

cursorResponse_Ok(BackEndPacket,_DbConn) :-
	cursorResponse_packet(CursorName,BackEndPacket),
	debug_message("Received CursorResponse, cursor name is ~w",[CursorName]),
	!.
cursorResponse_Ok(BackEndPacket,_DbConn) :-
	errorResponse_packet(ErrorMessage, BackEndPacket),
	!,
	error_message("postgres backend message (query failed): \n~s",[ErrorMessage]),
	fail.

cursorResponse_Ok(BackEndPacket,dbconnection(Stream,BackEndPID,BackEndKey)) :-
	noticeResponse_packet(NoticeMessage, BackEndPacket),
	!,
	warning_message("postgres backend message (processing a query): \n~s",[NoticeMessage]),
	socket_recv(Stream,NextPacket),
	cursorResponse_Ok(NextPacket,dbconnection(Stream,BackEndPID,BackEndKey) ).

cursorResponse_Ok(BackEndPacket,_DbConn) :-
	error_message("Unexpected packet ('CursorResponse' packet expected): ~s",[BackEndPacket]),
	fail.

%rowDescription_Ok(NumberOfFields,FieldsDescrList,BackEndPacket) :-
%	rowDescription_packet(NumberOfFields,FieldsDescrList,BackEndPacket),
%	!.
%rowDescription_Ok(NumberOfFields,FieldsDescrList,BackEndPacket) :-
%	errorResponse_packet(ErrorMessage, BackEndPacket),
%	!,
%	error_message("postgres backend message (query failed): \n~s",[ErrorMessage]),
%	fail.
%rowDescription_Ok(NumberOfFields,FieldsDescrList,BackEndPacket) :-
%	noticeResponse_packet(NoticeMessage, BackEndPacket),
%	!,
%	warning_message("postgres backend message (processing a query): \n~s",[NoticeMessage]),
%	socket_recv(Stream,NextPacket),
%	rowDescription_Ok(NextPacket).

%rowDescription_Ok(NumberOfFields,FieldsDescrList,BackEndPacket) :-
%	error_message("Unexpected packet ('RowDescription' packet expected)"),
%	fail.
