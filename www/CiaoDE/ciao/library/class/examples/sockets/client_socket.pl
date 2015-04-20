%%----------------------------------------------------------------------------
%% HANDLING SOCKETS USING O'CIAO
%%----------------------------------------------------------------------------

:- class(client_socket).

:- inherit_class(oo_socket).

:- use_module(library(sockets)).
:- use_module(library(system),[current_host/1]).

:- public [
	is_active/0,
	send/1,
	recv/1,
	recv_code/2,
	connect/2,
	connect/1,
	port/1,
	disconnect/0
	  ].

:- inheritable socket_id/1.

:- data current_port/2.

:- set_prolog_flag(multi_arity_warnings,off).

connect(Port) :-
	current_host(Host),
	connect(Host,Port).

connect(Host,Port) :-
	connect_to_socket(Host,Port,Socket),
	asserta_fact(socket_id(Socket)),
	asserta_fact(current_port(Port,Host)).

:- set_prolog_flag(multi_arity_warnings,on).

port(P) :-
	current_port(P,_).

host(H) :-
	current_port(_,H).
