
:- module(on_http,
	[ http/3, client/1, server/1, close/1 ],
	[ assertions, basicmodes, hlc, regtypes ]).

:- use_module(library(read),[read/2]).
:- use_module(library(sockets)).
:- use_module(library('sockets/sockets_io'),[safe_write/2,serve_socket/3]).
:- use_module(library(system),[current_host/1]).

:- true pred http(+ConnId,+Message,-Response) : service_connection(ConnId)
     # "Facts asserted to this predicate will result in writing a
        message with contents @var{Message} to the output connection
        @var{ConnId}, and wait for an answer. The fact triggering the
        writing of the message is erased and replaced by another one with
        the contents of the answer in @var{Response}.".

:- concurrent http/3.
:- data im_client_on/2, im_server_on/1.

:- true pred client(+ConnId) : service_connection(ConnId)
     # "Opens an output connection on @var{ConnId}.".

client(Host:Port):-
	current_fact(im_client_on(Port,Host)), !.
client(Host:Port):-
	asserta_fact(im_client_on(Port,Host)),
	send(Host:Port) && .
%% Corrected by Edison Mera
% send(Service):-
send(Host:Port) :-
	retract_fact(http(Host:Port,Document,Request,_)),
	fetch_url(http(Host,Port,Document),Request,Response),
	asserta_fact(http(Host:Port,Document,Request,Response)),
	fail.
send(_Service).

:- true pred server(-ConnId) => service_connection(ConnId)
     # "Opens an input connection on @var{ConnId}.".

server(Host:Port):-
	current_host(Host),
	bind_socket(Port,5,Socket),
	assertz_fact(im_server_on(Port)), !,
	serve_socket(Socket,serve,broken) && .

serve(String,Stream):-
	assertz_fact(receive(Stream,String)).

broken(_Stream). % connection closed

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
