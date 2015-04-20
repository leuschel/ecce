:- use_module(library(fastrw)).
:- use_module(library(sockets)).
:- use_module(library(concurrency)).

:- include(port).

main :-
	Term = f(a),
	port(Port),
	connect_to_socket(localhost, Port, Stream),
	fast_write(Stream, Term),
	fast_read(Stream, Ack),
	display(Ack), nl.
