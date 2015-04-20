:- module( vrml_sockets, 
	[open_socket/1,
	 open_socket/2] ).

:- include(library(assertions)).
:- include(library(basicmodes)).
:- include(library(types)).

:- use_module(library(lists), [append/3]).
:- use_module(library(system)).
:- use_module(library(sockets)).


:- comment(title,"The vrml sockets/system implementation.").

:- comment(module,"This module provides predicates to open sockets and
	           to establish contact with given port,host. If the portnumber
	           is unknown but are sent over the net, this will be read 
	           from the opend, in our case application.

                   Implemented by G@..{o}ran Smedb@..{a}ck").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

script_place( '/home/goran/VRML/CV3D/vis' ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred open_socket(-Stream, +Port_number)
   :: stream * int
   ; "This predicate will open a connection through a socket at the 
      given port number, and return the
      Stream to the socket. A Stream for read and write.".

open_socket(Stream, Port) :-
	!,
	get_hostname(Host),
	connect_to_socket(Host, Port, Stream).

%%%%%%%%%%%%%%%%
:- pred open_socket(-Stream )
   :: stream * int
 ; "This predicate will open a connection through a socket, the port number 
    will be sent trough the std_out for the predicate to read. Will then return
    the Stream to the socket. A Stream for read and write.".

open_socket(Stream) :-
	!,
	script_place(SP),
	get_hostname(Host),
	open_script(SP, Host, Stream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
open_script(ScriptPlace, Host, Stream) :-
	system_call(ScriptPlace, Stream0),
	get_portnumber(Stream0, Port),
	connect_to_socket(Host, Port, Stream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_portnumber(S, P) :-
	get_portnumber(S, [], P).

get_portnumber(_, [-1], stream_ended_before_number) :-
	!.

get_portnumber(S, [0'@], Port):-
	get_code(S, C),
	get_portnumber_rest(S, [0'@,C], Port).

get_portnumber(S, _, Port) :-
	get_code(S, C),
	get_portnumber(S, [C], Port).

get_portnumber_rest(S, [0'@,0'P,0'O,0'R,0'T,0'@], Port) :-
	!,
	get_code(S, C1),
	get_code(S, C2),
	get_code(S, C3),
	get_code(S, C4),
	name(Port, [C1,C2,C3,C4]).

get_portnumber_rest(S, Val, Port):-
	get_code(S, C),
	append(Val, [C], Rest),
	get_portnumber_rest(S, Rest, Port).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_hostname(Name) :-
	current_host(Name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_system_answer(Stream, Ans) :-
	get_code(Stream, C),
	get_answer(Stream, [C], [], Ans).

get_answer(_Stream, [-1], AnsCode, Ans) :-
	!,
	name(Ans, AnsCode).
	
get_answer(S, InCode, [InCode|AnsCode], Ans) :-
	get_code(S, C),
	get_answer(S, C, AnsCode, Ans).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
system_call(Call, Stream) :-
	popen(Call, read, Stream).
