
:- module(soap,[soap_send/2,soap_receive/2,client/2,server/2,close/1],
	       [assertions,basicmodes,hlc]).

:- use_module(library('pillow/html'),[xml2terms/2]).
:- use_module(library('pillow/pillow_types'),[canonic_xml_term/1]).
:- use_module(library('soap/xmlterm'),[canonic_xml_terms_filter/2]).
:- use_module(library(sockets)).
:- use_module(library('sockets/sockets_io'),[safe_write/2,serve_socket/3]).
:- use_module(library(system),[current_host/1]).

:- true pred soap_send(+ClientId,+Message)
     # "Facts asserted to this predicate will result in writing a SOAP
        message with contents @var{Message} to the output connection
        @var{ClientId}.".

:- true pred soap_receive(+SenderId,+Message) : canonic_xml_term(Message)
     # "Facts are asserted to this predicate when a SOAP message with
        contents @var{Message} are received in an input connection
        from @var{SenderId}. This one can be used to write back to the
        sender of the message".

:- concurrent soap_send/2, soap_receive/2.
:- data im_client_on/1, im_server_on/1.

:- true pred close(+SideId)
     # "Closes the connection @var{SideId}, either input or output, if
	it is open. If there is no such open connection, does nothing.".

close(Stream):-
	retract_fact(im_client_on(Stream)), !,
	streams_basic:close(Stream),
	( im_client_on(_Any) -> true
	; close_predicate(soap_send(_,_))
	).
close(Socket):-
	retract_fact(im_server_on(Socket)), !,
	( im_server_on(_Any) -> true
	; close_predicate(soap_receive(_,_))
	).
close(_Any).

:- true pred client(+ConnId,-ClientId)
     # "Opens an output connection on @var{ConnId}. It can be written
	to and closed using @var{ClientId}.".

client(Host:Port,Stream):-
	connect_to_socket(Host,Port,Stream),
	asserta_fact(im_client_on(Stream)),
	send(Stream) && .

:- true pred server(-ConnId,-ServerId)
     # "Opens an input connection on @var{ConnId}. It can be closed
	using @var{ServerId}.".

server(Host:Port,Socket):-
	current_host(Host),
	bind_socket(Port,5,Socket),
	assertz_fact(im_server_on(Socket)), !,
	serve_socket(Socket,serve,broken) && .

serve(String,Stream):-
	soap_message(String,Request), !,
	assertz_fact(soap_receive(Stream,Request)).
serve(String,_Stream):-
	throw(invalid_soap(String)).

send(Stream):-
	current_fact(soap_send(Stream,Request)),
	send_soap(Stream,Request),
	fail.
send(_Stream).

send_soap(Stream,Request):-
	soap_message(String,Request), !,
	safe_write(Stream,String).
send_soap(Stream,Request):- % will never happen...
	throw(invalid_xml(Stream,Request)).

broken(_Stream). % connection closed

soap_message(XMLSoap,Request):-
	nonvar(XMLSoap), !,
	soap_message_1(XMLSoap,Request).
soap_message(XMLSoap,Request):-
	nonvar(Request), !,
	soap_message_2(Request,XMLSoap).

soap_message_1(String,Request):-
	% It is XML code
	xml2terms(String,Terms),
	canonic_xml_terms_filter(Terms,[env(Envelope,Attr,Elements)]),
	% It is a SOAP envelope
	member(XmlNS="http://schemas.xmlsoap.org/soap/envelope/",Attr),
	atom_concat('xmlns:',NameSpace,XmlNS),
	atom_concat(NameSpace,':Envelope',Envelope),
	% It has a SOAP body
	parse_envelope(Elements,NameSpace,[Request|_Body]).
	% First body element is the request

parse_envelope([env(Name,_Attr,_SubEls)|Els],NameSpace,Body):-
	atom_concat(NameSpace,':Header',Name), !,
	parse_envelope(Els,NameSpace,Body).
parse_envelope([env(Name,_Attr,SubEls)|_Els],NameSpace,Body):-
	atom_concat(NameSpace,':Body',Name), !,
	Body=SubEls.

soap_message_2(Request,String):-
	soap_envelope(Envelope,Request),
	xml2terms(String,Envelope).

soap_envelope(env('SOAP-ENV:Envelope',
	 ['xmlns:SOAP-ENV'="http://schemas.xmlsoap.org/soap/envelope/",
	  'SOAP-ENV:encodingStyle'="http://schemas.xmlsoap.org/soap/encoding/"],         [env('SOAP-ENV:Body',
	  [],
	  [Request])
	 ]),	
	 Request).
