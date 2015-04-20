
/* Client
   It has to be run as "client Host Port File", then connects to a server
   in Host:Port and sends to it the contents of File.
   It receives an answer back confirming the reception of the message sent.
*/

% imports required to be a client
:- use_module(soap_client).
:- use_module(library(sockets)).

% imports required by the application itself
:- use_module(library(file_utils),[file_to_string/2]).
:- use_module(library('pillow/html'),[xml2terms/2]).

main([Host,P,Source]):-
	atom_codes(P,N),
	number_codes(Port,N),
	connect_to_socket(Host,Port,Stream),
	file_to_string(Source,String),
	xml2terms(String,XmlTerm),
	show(XmlTerm),
	soap_send(Stream,XmlTerm,Answer),
	display('SOAP answer:'), nl,
	show(Answer).

show(Message):-
	prolog_flag(write_strings,X,on),
	writeq(Message), nl,
	prolog_flag(write_strings,_,X).
