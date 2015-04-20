%%----------------------------------------------------------------------------
%% HANDLING SOCKETS USING O'CIAO
%%----------------------------------------------------------------------------

:- class(oo_socket).

:- use_module(library(sockets)).

:- data socket_id/1.

:- inheritable [socket_id/1,send/1,recv/1,recv_code/2,disconnect/0].

:- export(is_active/0).

is_active :-
	socket_id(_).

send(Data) :-
	socket_id(Socket),
	socket_send(Socket,Data).

recv(Data) :-
	socket_id(Socket),
	socket_recv(Socket,Data).

recv_code(Code,Lenght) :-
	socket_id(Socket),
	socket_recv_code(Socket,Code,Lenght).

destructor :-
	disconnect.

disconnect :-
	retract_fact(socket_id(Socket)),
	close(Socket).
