:- module(socket_serve,
	[ serve_socket_mono/3, serve_socket_multi/2 ],
	[ assertions, hiord, hlc ]
	 ).
:- use_module(library(lists),[delete/3]).
:- use_module(library(read),[read/2]).
:- use_module(library(sockets),[select_socket/5]). 

:- true pred serve_socket_mono(Socket,Server,Handler)
	: socket * callable * callable
        # "Handles the streams associated to @var{Socket} calling
           @var{Server} on one request of each stream 
           (as @var{Server(Request,Stream)}),
           and @var{Handler} if the stream is empty (broken connection).".
:- meta_predicate serve_socket_mono(?,pred(2),goal).

serve_socket_mono(Socket,Server,Handler) :-
	serve_socket_1(Socket,Server,Handler,[]).

serve_socket_1(Socket,Server,Handler,Streams0) :-
	wait_for_arrival(Socket,Streams0,ReadableStreams,Streams1),
	serve_streams(ReadableStreams,Server,Handler,Streams1,Streams2),
	serve_socket_1(Socket,Server,Handler,Streams2).

:- true pred serve_socket_multi(Socket,Server) : socket * callable 
        # "Handles the streams associated to @var{Socket} calling
           @var{Server} on each stream (@var{Server(Stream)})
           in a separate thread. @var{Server} is responsible from then on
           of handling the stream.".
:- meta_predicate serve_socket_multi(?,pred(1)).

serve_socket_multi(Socket,Server) :-
	serve_socket_2(Socket,Server,[]).

serve_socket_2(Socket,Server,Streams0) :-
	wait_for_arrival(Socket,Streams0,ReadableStreams,Streams1),
	serve_streams_thread(ReadableStreams,Server,Streams1,Streams2),
	serve_socket_2(Socket,Server,Streams2).

/* Waits for either one or all of the following:
	1) A connection is done to 'Socket'
        2) It is possible to read from a stream
*/
wait_for_arrival(Socket,Streams0,ReadableStreams,Streams) :-
	select_socket(Socket,NewStream,off,Streams0,ReadableStreams),
	new_stream_in_list(NewStream,Streams0,Streams).

new_stream_in_list(NewStream,Streams,Streams) :-
	var(NewStream),!.
new_stream_in_list(NewStream,Streams0,[NewStream|Streams0]).

serve_streams([],_S,_H,SS,SS).
serve_streams([Stream|Streams],Server,Handler,SS0,SS) :-
	read(Stream,Request),
	serve_one_stream(Request,Stream,Server,Handler,SS0,SS1),
	serve_streams(Streams,Server,Handler,SS1,SS).

serve_one_stream(end_of_file,S,_Server,Handler,SS0,SS) :- !,
        % end of file,that is,a broken connection
	Handler,
	close(S),
	delete(SS0,S,SS).
serve_one_stream(Request,Stream,Server,_Handler,SS,SS) :-
	Server(Request,Stream).

serve_streams_thread([],_S,SS,SS).
serve_streams_thread([Stream|Streams],Server,SS0,SS) :-
	Server(Stream), % && ,
	delete(SS0,Stream,SS1),
	serve_streams_thread(Streams,Server,SS1,SS).
