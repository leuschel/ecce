:- module( vrml_socket_communication, 
	[socket_call/2, 
	 open_connection/1,
	 open_connection/2] ).

:- include(library(assertions)).
:- include(library(basicmodes)).
:- include(library(types)).

:- use_module(library(lists), [append/3]).
:- use_module(library(system)).
:- use_module(library(sockets)).
:- use_module(vrml_sockets).


:- comment(title,"The vrml so").

:- comment(module,"This module provides predicates to open sockets and
	           to establish contact with given port,host. If the portnumber
	           is unknown but are sent over the net, this will be read 
	           from the opend, in our case application.

                   Implemented by G@..{o}ran Smedb@..{a}ck").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred socket_call(+Stream, +Command)
   :: stream * Command
   ; "".

socket_call(Stream, close(Num)) :-
	name(Num, ListOfNum),
	append("close",ListOfNum,Send1),
	append(Send1, "\n",Send),
	socket_send(Stream, Send).

socket_call(Stream, exit) :-
	socket_send(Stream, "exit\n"),
	close(Stream).

socket_call(Stream, code(Num, Msg)) :-
	name(Num, ListOfNum),
	append("code",ListOfNum,Send1),
	append(Send1, "\n",Send3),
	append(Send3,Msg,Send2),
	get_end_of_msg(End),
	append(Send2, End, Send),
	socket_send(Stream, Send).

socket_call(Stream, code(Msg)) :-
	append("code",Msg,Send1),
	get_end_of_msg(End),
	append(Send1, End, Send),
	socket_send(Stream, Send).

socket_call(Stream, file(Msg)) :-
	append("file",Msg,Send1),
	append(Send1, "\n", Send),
	socket_send(Stream, Send).

socket_call(Stream, change(Num, Code)) :-
	name(Num, ListOfNum),
	append("change\n",ListOfNum,Send0),
	append(Send0, "\n", Send1),
	append(Send1, Code, Send2),
	get_end_of_msg(End),
	append(Send2, End, Send),
	socket_send(Stream, Send).

socket_call(Stream, rotate(Frame, Scene)) :-
	name(Frame, ListOfFrame),
	name(Scene, ListOfScene),
	append("rotate\n",ListOfFrame,Send0),
	append(Send0, "\n", Send1),
	append(Send1, ListOfScene, Send2),
	get_end_of_msg(End),
	append(Send2, End, Send),
	socket_send(Stream, Send).

socket_call(Stream, rotate_back(Frame)) :-
	name(Frame, ListOfFrame),
	append("rotate_back\n",ListOfFrame,Send0),
	append(Send0, "\n", Send1),
	get_end_of_msg(End),
	append(Send1, End, Send),
	socket_send(Stream, Send).

socket_call(Stream, rotate_forth(Frame)) :-
	name(Frame, ListOfFrame),
	append("rotate_forth\n",ListOfFrame,Send0),
	append(Send0, "\n", Send1),
	get_end_of_msg(End),
	append(Send1, End, Send),
	socket_send(Stream, Send).

socket_call(Stream, new) :-
	socket_send(Stream, "new\n").



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_end_of_msg("\n@END@\n").
get_start_of_msg("\n@START@\n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
open_connection(Stream) :-
	open_socket(Stream).
open_connection(Stream,P) :-
	open_socket(Stream,P).
