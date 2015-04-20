
:- module(on_sockets,
	[ send/2, receive/2, client/2, server/2, close/1,
	  client_readback/1, server_writeback/1 ],
	[ assertions, basicmodes, hlc, regtypes ]).

:- use_module(library(read),[read/2]).
:- use_module(library(sockets)).
:- use_module(library('sockets/sockets_io'),[safe_write/2,serve_socket/3]).
:- use_module(library(system),[current_host/1]).

:- true pred send(+ClientId,+Message) : stream(ClientId)
     # "Facts asserted to this predicate will result in writing a
        message with contents @var{Message} to the output connection
        @var{ClientId}. This one can be used to read back from the
        receiver of the message.".

:- true pred receive(+SenderId,+Message) : stream(SenderId)
     # "Facts are asserted to this predicate when a message with
        contents @var{Message} are received in an input connection
        from @var{SenderId}. This one can be used to write back to the
        sender of the message".

:- concurrent send/2, receive/2.
:- data im_client_on/1, im_server_on/1.

:- true pred client(+ConnId,-ClientId)
     : service_connection(ConnId) => stream(ClientId)
     # "Opens an output connection on @var{ConnId}. It can be written
	to and closed using @var{ClientId}.".

client(Host:Port,Stream):-
	connect_to_socket(Host,Port,Stream),
	asserta_fact(im_client_on(Stream)),
	send(Stream) && .

:- true pred server(-ConnId,-ServerId)
     => ( service_connection(ConnId), server_id(ServerId) )
     # "Opens an input connection on @var{ConnId}. It can be closed
	using @var{ServerId}.".

server(Host:Port,Socket):-
	current_host(Host),
	bind_socket(Port,5,Socket),
	assertz_fact(im_server_on(Socket)), !,
	serve_socket(Socket,serve,broken) && .

serve(String,Stream):-
	assertz_fact(receive(Stream,String)).

broken(_Stream). % connection closed

:- true pred server_writeback(+SenderId) : stream(SenderId)

     # "Enables a server to write messages back to senders connected 
	to @var{SenderId}.".

server_writeback(Stream):-
	send(Stream) && .

send(Stream):-
	current_fact(send(Stream,String)),
	safe_write(Stream,String),
	fail.
send(_Stream).

:- true pred client_readback(+ClientId) : stream(ClientId)
     # "Enables a client to read messages back from the (output) connexion
	on @var{ClientId}.".

client_readback(Stream):-
	receive(Stream) && .

receive(Stream):-
	repeat,
	catch(read(Stream,Message),_,broken(Stream)),
	assertz_fact(receive(Stream,Message)),
	fail.

:- comment(close(SideId),"Closes the connection @var{SideId}, either input
	or output, if it is open. If there is no such open connection, does
        nothing.").
:- true pred close(+SideId) : stream(SideId)
	# "Closes (client) output connection @var{SideId}.".
:- true pred close(+SideId) : server_id(SideId)
	# "Closes (server) input connection @var{SideId}.".

close(Stream):-
	retract_fact(im_client_on(Stream)), !,
	streams_basic:close(Stream),
	( im_client_on(_Any) -> true
	; close_predicate(send(_,_))
	).
close(Socket):-
	retract_fact(im_server_on(Socket)), !,
	( im_server_on(_Any) -> true
	; close_predicate(receive(_,_))
	).
close(_Any).

:- comment(doinclude,service_connection/1).
:- regtype service_connection/1.
:- comment(service_connection/1,"@includedef{service_connection/1}").

service_connection(Host:Port):- atom(Host), number(Port).

:- comment(doinclude,server_id/1).
:- regtype server_id/1.
:- comment(server_id/1,"@includedef{server_id/1}").

server_id(Id):- number(Id).
