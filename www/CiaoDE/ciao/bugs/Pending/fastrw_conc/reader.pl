:- use_module(library(fastrw)).
:- use_module(library(sockets)).
:- use_module(library(concurrency)).

:- include(port).

main :-
	port(Port),
	bind_socket(Port, 10, Socket),
% Using threads prevents writer from getting the ack:
	eng_call(do_serve(Socket), create, create). 
% This works:
%	do_serve(Socket).

do_serve(Socket) :-
	socket_accept(Socket, Stream),
	fast_read(Stream, Term),
	display(Term), nl,
	fast_write(Stream, ack),
	display(ack_sent).
