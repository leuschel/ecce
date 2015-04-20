%%----------------------------------------------------------------------------
%% HANDLING SOCKETS USING O'CIAO
%%----------------------------------------------------------------------------

:- class(server_socket).

:- inherit_class(oo_socket).

:- use_module(library(sockets)).

:- public [
	is_active/0,
	send/1,
	recv/1,
	recv_code/2,
	port/1,
	wait_connection/0,
	disconnect/0].

:- inheritable [current_port/2,socket_id/1].

:- data       current_port/2.

:- set_prolog_flag(multi_arity_warnings,off).

server_socket :-
	bind_socket(Port,1,Socket),
	asserta_fact(current_port(Port,Socket)).

server_socket(Port) :-
	bind_socket(Port,1,Socket),
	asserta_fact(current_port(Port,Socket)).

port(P) :-
	current_port(P,_).

wait_connection :-
	current_port(_,Socket),
	socket_accept(Socket,Stream),
	asserta_fact(socket_id(Stream)).
