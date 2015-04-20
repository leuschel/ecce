
/* Server
   It should be invoked as "server Num"
   It will start a server process bound to socket Num that will receive
   messages on that port. The server id is thus www.clip.dia.fi.upm.es:Num

   The application protocol is supposed to be asynchronous, so that only
   a confirmation of reception is sent back. In a synchronous protocol 
   a true response could be sent back altogether.

   In the present case, a true response, if any, would have to be sent
   as a different message. To send such messages the client part should 
   be used. See client.pl
*/

% imports required by the processing of the server
%:- use_module(library('pillow/http_server')).
:- use_module(http_server).
:- use_module(library(sockets)).
:- use_module(library('sockets/sockets_io'),[serve_socket/3]).
:- use_module(library(soap),[soap_message/2]).

% imports required by the processing of the application itself
:- use_package(persdb).
:- use_module(library(strings),[write_string/2]).
:- use_module(library(write)).

% persistent predicates
% these are used to assert messages received and errors detected
% for debugging purposes; they can be found as files:
% ./user/cached_1.pl  and  ./user/received_message_2.pl
persistent_dir(db,'./').

:- persistent(received_message/2,db).
:- persistent(cached/1,db).

% main entry point
% for each request received from a client predicate server/1 is called;
% errors are catched by predicate catcher/1
main([S]):-
	atom_codes(S,Codes),
	number_codes(N,Codes),
	set_prolog_flag(write_strings,on),
	bind_socket(N,5,Socket),
	serve_socket(Socket,server,catcher).

% request handler
% request is attended, request message is processed by serve/3, which
% elaborates and answer; this one is sent back and the conexion is 
% closed thereafter (because of the HTTP protocol)
server(Stream):-
	http_serve_fetch(Stream,serve(Stream)).

serve(Message,Stream,Answer):-
	writeq(received_message(Stream,Message)), nl,
	assertz_fact(received_message(Stream,Message)),
	process(Message,Answer).

% error handler
% any error found during request handling will raise an exception
% which is catched by this predicate
catcher(Error):- 
	assertz_fact(cached(Error)),
	writeq(Error), nl.

% application processing
% parses the HTTP message and extracts the contents of the SOAP message,
% yielding an XML term; it builds an OK answer or an error if messages
% cannot be parsed
process(Message,[Answer]):-
	http_message(Message,SoapStr),
	soap_message(SoapStr,XmlTerm), !,
	http_answer(good,Answer),
	% show the message received
	writeq(xmlterm(XmlTerm)), nl.
process(_Message,[Answer]):-
	http_answer(bad,Answer).

http_answer(bad,status(request_error,400,"ERROR")).
http_answer(good,status(success,200,"OK")).

http_message(Message,SoapStr):-
	member(post,Message), !,
	member(content(SoapStr),Message).
http_message(Message,SoapStr):-
	member(document(SoapStr),Message).
