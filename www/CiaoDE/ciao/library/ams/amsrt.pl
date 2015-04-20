
:- module(amsrt,[ams_call/2,ams_service/1,ams_shutdown/1,ams_startup/6],[]).
:- use_module(library(read)).
:- use_module(library(sockets)). 

debug(M):- display(user_error,M), nl(user_error).
%debug(_).

%throw(Error):- error(Error), abort.

%jcf% ams_service(4999).
ams_service(49999).

ams_sid(sid(Host,Port),Host,Port).

ams_mysid(mysid(Host,Port,Stream0,Stream1),Host,Port,Stream0,Stream1).

actmod_or_daemon_port(a(Port),Port):- !.
actmod_or_daemon_port(Port,Port).

ams_startup(Host,Aid,Mod,Mid,Mode,MySid):-
	put_request_server(0,Host,startup(Aid,Mod,Mid,Mode),Stream),
	amsrt:debug(reading),
	read(Stream,MyPort),
	answer(MyPort),
	amsrt:debug(ready_to_connect(MyPort)),
	actmod_or_daemon_port(MyPort,Port),
	connect_to_socket(Host,Port,MyStream),
	ams_mysid(MySid,Host,MyPort,MyStream,Stream).

:- data already_shut/1.

ams_shutdown(Sid):-           % for some reason, upon ams exceptions, this 
	already_shut(Sid), !. % guy was being called over and over
                              % until it made the server die...
ams_shutdown(Sid):-
	ams_mysid(Sid,Host,Port,_S,Stream0),
	put_request_server(Stream0,Host,shutdown(Port),Stream),
	read(Stream,Answer),
	close(Stream),
	asserta_fact(already_shut(Sid)),
	answer(Answer).

ams_call(MySid,Goal):-
	ams_mysid(MySid,Host,Port,Stream,_S),
	actmod_or_daemon(Port,Host,Stream,Goal).

actmod_or_daemon(a(Port),Host,Stream,Goal):- !, % for active modules
	ams_sid(Sid,Host,Port),
	put_request(Stream,Sid,Goal),
	read(Stream,Answer),
	answer(Answer),
	member(Goal,Answer).
actmod_or_daemon(Port,Host,Stream,Goal):-
	ams_sid(Sid,Host,Port),
	solve(Stream,Sid,Goal).

solve(Stream,Sid,Goal):-
	put_request(Stream,Sid,Goal),
	read(_),
	read(Stream,Answer),
	answer(Answer),
	Answer=Goal.  % Goal\==0
solve(Stream,Sid,Goal):-
	repeat,
	  put_request(Stream,Sid,0),
	  read(Stream,Answer),
	  ( Answer=0, !,
	    fail
	  ; answer(Answer),
	    Answer=Goal
	  ).

answer(end_of_file):- !,
	throw(ams(connection_closed)).
answer(Answer):-
	number(Answer),
	Answer < 0, !,
	throw(ams(connection_error(Answer))). % translate Answer to something!
answer(_Answer):-
	amsrt:debug(answer(_Answer)).

put_request(Stream,_Sid,Term):-
        current_stream(_N,socket,Stream), !,
	send(Stream,Term).
put_request(_Stream,Sid,Term):-
	ams_sid(Sid,Host,Port),
	throw(ams(unable_to_connect(Host,Port,Term))).

put_request_server(Stream0,_H,Term,Stream):-
        current_stream(_N,socket,Stream0), !,
	send(Stream0,Term),
	Stream = Stream0.
put_request_server(_S0,Host,Term,Stream):-
	ams_service(Port),
	connect_to_socket(Host,Port,Stream),
	send(Stream,Term).

send(Stream,Term):-
	amsrt:debug(ready_to_send(Term)),
	current_output(OldOut),
	set_output(Stream),  % should not fail!
	display_term(Term),
	flush_output,
	set_output(OldOut).
